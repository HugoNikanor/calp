(define-module (calp server webdav)
  :use-module ((hnh util) :select (for group -> ->> init+last catch* print-and-return))
  :use-module (hnh util lens)
  :use-module (hnh util another-logger)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (ice-9 format)
  :use-module (ice-9 control)
  :use-module (ice-9 curried-definitions)
  :use-module (web request)
  :use-module (web response)
  :use-module (web uri)
  :use-module (web server)
  :use-module ((web http) :select (declare-method!
                                   declare-header!))
  :use-module (web http status-codes)
  :use-module (datetime)
  :use-module (sxml match)
  :use-module (sxml namespaced)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (rnrs bytevectors)
  :use-module (rnrs io ports)
  :use-module (calp namespaces)
  :use-module (calp webdav resource)
  :use-module (calp webdav href)
  :use-module (calp webdav property)
  :use-module (calp webdav propfind)
  :use-module (calp webdav proppatch)
  :use-module (calp webdav util)
  :use-module ((rnrs base) :select (assert) :version (6))
  :export (
           run-propfind
           run-proppatch
           run-options
           run-get
           run-put
           run-delete
           run-mkcol
           run-copy
           run-move
           run-report

           webdav-handler

           current-log-port
           ))




(declare-method! 'PROPFIND)
(declare-method! 'PROPPATCH)
(declare-method! 'MKCOL)
(declare-method! 'COPY)
(declare-method! 'MOVE)
(declare-method! 'LOCK)
(declare-method! 'UNLOCK)
(declare-method! 'REPORT)
(declare-method! 'MKCALENDAR)



;; Errors thrown during header parse are outputed to the console,
;; and the HTTP request is fulfilled with a 400 Bad Request with an
;; empty body.

(declare-header! "DAV"
  parse-dav-line
  validate-dav-line
  write-dav-line)

(declare-header! "Depth"
  (lambda (str)
    (if (string-ci=? str "Infinity")
        'infinity
        (or (string->number str)
            (scm-error 'misc-error "Depth header"
                       "Invalid value for depth header: ~s"
                       (list str) '()))))
  (lambda (value)
    (memv value '(0 1 infinity)))
  (lambda (value port)
    (display value port)))

(declare-header! "Destination"
  (lambda (s)
    (or (string->uri s)
        (build-uri 'http path: s)))
  uri?
  (lambda (uri port)
    (display (uri->string uri) port)))

;;; TODO
;; (declare-header! "If")

;;; TODO
;; (declare-header! "Lock-Token")

(declare-header! "Overwrite"
  (lambda (str)
    (assert (= 1 (string-length str)))
    (case (string-ref str 0)
      ((#\F) #f)
      ((#\T) #t)
      (else (throw 'error))))
  boolean?
  (lambda (b port)
    (display (if b "T" "F")
             port)))

;;; TODO
;; (declare-header! "Timeout")



;;; TODO integrate this into a true log system
(define current-log-port (make-parameter (current-error-port)))



(define (run-propfind root-resource href request body)
  (define headers (request-headers request))
  (cond ((lookup-resource root-resource href)
         => (lambda (resource)
              ;; A list of (path, resource) pairs
              (define requested-resources
                (case (or (assoc-ref headers 'depth) 'infinity)
                  ((0) (list (cons href resource)))
                  ((1) (cons (cons href resource)
                             (map (lambda (pair)
                                    (modify pair car* (lambda (name) (append href (list name)))))
                                  (children resource))))
                  ((infinity) (all-resources-under resource href))))


              ;; Body, if it exists, MUST have be a DAV::propfind object
              (define property-request
                (xml-document-root
                 (cond ((string? body)
                        (xml->namespaced-sxml body))
                       ((bytevector? body)
                        (-> body
                            (bytevector->string
                             ;; TODO check content type
                             (make-transcoder (utf-8-codec)))
                            xml->namespaced-sxml))
                       (else (xml-document
                              root: ((xml webdav 'propfind)
                                     ((xml webdav 'allprop))))))))

              (values
               (build-response code: 207
                               reason-phrase: (http-status-phrase 207)
                               headers: '((content-type . (application/xml))))
               (apply
                (xml webdav 'multistatus)
                (for (href . resource) in requested-resources
                     (apply (xml webdav 'response)
                            ((xml webdav 'href) (href->string href))
                            (map propstat->namespaced-sxml
                                 (exec-propfind property-request resource))))))))

        (else (values (build-response code: 404)
                      "Failed finding child"))))



(define (run-proppatch root-resource href request body)
  (cond ((lookup-resource root-resource href)
         => (lambda (resource)
              ;; Body MUST exist, and be a DAV::propertyupdate element
              (define request-body
                (xml-document-root
                 (xml->namespaced-sxml
                  (cond ((string? body) body)
                        ((bytevector? body)
                         (bytevector->string body (make-transcoder (utf-8-codec))))
                        (else (throw 'http 400 "A body is required for proppatch"))))))

              (values (build-response
                       code: 207
                       reason-phrase: (http-status-phrase 207)
                       headers: '((content-type . (application/xml))))

                      ((xml webdav 'multistatus)
                       (apply
                        (xml webdav 'response)
                        ((xml webdav 'href) (href->string href))
                        (map propstat->namespaced-sxml
                             (exec-propertyupdate request-body resource)))))))

        (else (build-response code: 404))))


(define (run-options root-resource href request)
  (cond ((lookup-resource root-resource href)
         => (lambda (resource)
              (build-response
               code: 200
               headers: `((dav . (1 3
                                    ;; TODO dispatch to method on resource
                                    ;; "calendar-access"
                                    ))

                          ;; TODO collecting this set dynamically would be fancy!
                          (allow . (GET HEAD PUT
                                        MKCOL PROPFIND OPTIONS
                                        DELETE
                                        COPY
                                        MOVE
                                        ;; LOCK
                                        ;; UNLOCK
                                        ;; TODO return REPORT where applicable
                                        ))))))
        (else (build-response code: 404))))

(define (run-get root-resource href request)
  (cond ((lookup-resource root-resource href)
         => (lambda (resource)
              (define-values (rendered ct)
               (call-with-values (lambda () (content resource (request-headers request)))
                 (case-lambda ((rendered)
                               (values rendered
                                       (and=> (content-type resource)
                                              (compose list string->symbol))))
                              ((rendered ct) (values rendered (list (string->symbol ct)))))))

              (values (build-response
                       code: 200
                       headers:
                       (filter cdr
                               `((content-type . ,ct)
                                 (last-modified . ,(and=> (last-modified resource)
                                                          (@ (datetime srfi-19) datetime->srfi-19-date)))
                                 (content-language . ,(content-language resource))
                                 (content-length . ,(content-length resource))
                                 (etag . ,(etag resource)))))
                      ;; Content will be filtered out by Guile's
                      ;; webserver for HEAD requests.
                      rendered)))

        (else (build-response code: 404))))


;;; TODO put this somewhere propper
(define (content-type->string ct)
  (string-append (symbol->string (car ct))
                 (string-concatenate
                  (map (lambda (p) (format #f ";~a=~a" (car p) (cdr p)))
                       (cdr ct)))))

(define (run-put root-resource href request request-body)

  ;; Helper procedure, since the code is shared between the creation
  ;; and update path.
  (define (update-resource! resource)
    (define etag (set-content! resource request-body (request-headers request)))
    (cond ((request-content-language request)
           (negate null?)
           => (lambda (content-language)
                (set-property! resource
                               ((xml webdav 'getcontentlanguage) (car content-language))))))

    (cond ((request-content-type request)
           => (lambda (content-type)
                (set-property! resource ((xml webdav 'getcontenttype)
                                         (content-type->string content-type))))))
    etag)

  ;; TODO handle If, If-Match, and similar headers

  (cond ((lookup-resource root-resource href)
         => (lambda (resource)
              (define etag (update-resource! resource))
              (build-response
               code: 204
               headers: `(,@(if etag `((etag ,etag)) '())))))

        ;; href will never be the empty list here, since the root
        ;; resource would have matched that beforehand.
        ((lookup-resource root-resource (drop-right href 1))
         => (lambda (parent)
              (let ((resource (create-resource! parent (last href))))
                (define etag (update-resource! resource))
                (build-response
                 code: 201
                 headers: `(,@(if etag `((etag ,etag)) '()))))))

        ;; No parent collection, fail per [WEBDAV] 9.7.1.
        (else (values (build-response
                       code: 409
                       headers: '((content-type text/plain)))
                      "Parent missing"))))


(define (run-mkcol root-resource href request body)
  (cond ((lookup-resource root-resource href)
         => (lambda (resource) (build-response code: 405)))
        ((lookup-resource root-resource (drop-right href 1))
         => (lambda (parent)
              (create-collection! parent (last href)
                                  (request-headers request) body)
              (build-response code: 201)))
        (else (build-response code: 409))))



(define (run-copy root-resource source-href request)
  (define headers (request-headers request))

  (define depth (or (assoc-ref headers 'depth) 'infinity))
  (define overwrite? (cond ((assoc 'overwrite headers) => cdr)
                           (else #t)))

  ;; TODO handle If, If-Match, and similar headers

  ;; TODO ensure a cross domain move isn't attempted

  (define-values (dest-path dest-name)
    (-> (or (assoc-ref headers 'destination)
            (throw 'http 400 "Missing Destination header"))
        uri-path string->href init+last))

  (build-response
   code: (let ((source-resource
                (cond ((lookup-resource root-resource source-href) => identity)
                      (else (throw 'http 404))))
               (destination-parent-resource
                (cond ((lookup-resource root-resource dest-path) => identity)
                      (else (throw 'http 409
                                   "One or more parent components of destination are missing")))))

           (case (copy-resource! source-resource destination-parent-resource dest-name
                                 depth: depth
                                 overwrite?: overwrite?)
             ((created)   201)
             ((replaced)  204)
             ((collision) 412)))))


(define (run-delete root-resource href request)
  (build-response
   code: (cond ((lookup-resource root-resource href)
                => (lambda (resource)
                     (remove-self! resource)
                     202))
               (else 404))))


(define (run-move root-resource href request)
  (define headers (request-headers request))

  (define-values (dest-path dest-name)
    (-> (or (assoc-ref headers 'destination)
            (throw 'http 400 "Missing Destination header"))
        uri-path string->href init+last))

  (define overwrite?
   (cond ((assoc 'overwrite headers) => cdr)
         (else #t)))

  ;; TODO ensure a cross domain move isn't attempted

  ;; TODO handle If, If-Match, and similar headers

  (build-response
   code: (cond ((lookup-resource root-resource href)
                => (lambda (source)
                     (cond ((lookup-resource root-resource dest-path)
                            => (lambda (destination)
                                 (case (move-resource!
                                        source destination dest-name
                                        overwrite?: overwrite?)
                                   ((created)  201)
                                   ((replaced) 204))))
                           (else 409))))
               (else 404))))



;; (define (run-report href request request-body))






;; For all headers:
;; `((server ,(format #f "calp/~a" (@ (calp) calp-version)))
;;   (date ,(datetime->string (current-datetime)
;;                            "~a, ~d ~b ~Y ~H:~M:~S GMT"))
;;   (connection keep-alive))

;; Already fixed by server
;;   (content-length ,(format #f (bytevector->length data)))


(define ((webdav-handler root-resource) request request-body)
  (format (current-log-port) "> ~a ~a~%> Headers:~%"
          (request-method request) (uri->string (request-uri request)))
  (for (header . value) in (request-headers request)
       (format (current-log-port) ">     ~a: ~s~%" header value))

  (define href (-> request request-uri uri-path
                   (uri-decode decode-plus-to-space?: #f)
                   string->href))
  ;; Initialize logger parameter
  (log-table (make-log-table))
  (log-table-add! 'now (current-datetime)
                  'method (request-method request)
                  'uri (request-uri request)
                  'headers (request-headers request)
                  'request request)

  (define-values (response body*)
    (catch*
     (lambda ()
       (call-with-values
           (lambda ()
             (case (request-method request)
               ((OPTIONS) (run-options root-resource href request))

               ((PROPFIND)  (run-propfind  root-resource href request request-body))
               ((PROPPATCH) (run-proppatch root-resource href request request-body))

               ((GET HEAD) (run-get root-resource href request))

               ((PUT) (run-put root-resource href request request-body))

               ((DELETE) (run-delete root-resource href request))

               ((MKCOL) (run-mkcol root-resource href request request-body))

               ((COPY) (run-copy root-resource href request))
               ((MOVE) (run-move root-resource href request))

               ;; ((REPORT))

               (else (build-response code: 400) "")))

         (case-lambda
           ((head)      (values head ""))
           ((head body) (values head body)))))

     (http
      (lambda* (_ error-code optional: (body "") content-type)
        (values (build-response code: error-code
                                headers: (if content-type
                                           `((content-type . content-type))
                                           '()))
                body)))

     ;; (wrong-type-arg
     ;;  (lambda (_ procedure msg args data)
     ;;    (log-table-add! 'msg (format #f "~?~%" msg args))
     ;;    (values (build-response code: 500
     ;;                            headers: `((content-type text/plain)))
     ;;            "Internal server error")))

     (parser-error
      (lambda (err port msg . args)
        (define head (build-response code: 400
                                     headers: '((content-type . (text/plain)))))
        (define errmsg
          (with-output-to-string
            (lambda ()
              (display msg)
              (for-each display args))))
        (log-table-add! 'msg errmsg)
        (values head errmsg)))

     ((pre-unwind #t)
      (lambda _ (log-table-add! 'backtrace (with-output-to-string (lambda () (backtrace))))))

     (#t
      (case-lambda ((err proc fmt args data)
                    (let ((head (build-response
                                 code: 500
                                 headers: '((content-type . (text/plain)))))
                          (errmsg (if proc
                                      (format #f "~a error in ~a: ~?~%" err proc fmt args)
                                      (format #f "~?~%" fmt args))))
                      (log-table-add! 'msg errmsg)
                      (values head errmsg)))
                   (err
                    (let ((errmsg (format #f "General error: ~s~%" err)))
                      (log-table-add! 'msg errmsg)
                      (values (build-response code: 500)
                              errmsg)))))))

  (log-table-add!
   'response response
   'response-code   (response-code response)
   'response-phrase (response-reason-phrase response))

  (emit-log! (current-log-port))

  ;; TODO
  ;; if no content type in response headers, insert one:
  ;; `((content-type
  ;;    . ,(cond (content-type list? => identity)
  ;;             (content-type => list)
  ;;             ((string? body) '(text/plain))
  ;;             ((xml-element? body) '(application/xml))
  ;;             (else '(application/octet-stream)))))

  (values response
          (cond ((xml-element? body*) (with-output-to-string
                                        (lambda ()
                                          (namespaced-sxml->xml
                                           (xml-document
                                            pi: (list (pi-element 'xml "version=\"1.0\" encoding=\"UTF-8\""))
                                            root: body*)
                                           namespaces: `((,xcal . IC)
                                                         (,webdav . D)
                                                         (,caldav . C)
                                                         (,calp-namespace . calp)
                                                         ))
                                          (newline))))
                (else body*))))

(define-module (calp server webdav)
  :use-module ((hnh util) :select (for group -> ->> init+last catch* print-and-return))
  :use-module (hnh util lens)
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
  :use-module (calp webdav property)
  :use-module (calp webdav propfind)
  :use-module (calp webdav proppatch)
  :use-module (calp webdav util)
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
           ))

;; (define (run-filter context filter-spec)
;;   (sxml-match filter-spec
;;               [(c:comp-filter (@ (name ,name)) . ,rest)
;;                ;; TODO
;;                (filter (lambda (child) (string=? name (type child)))
;;                        (children context))]
;;               [(c:prop-filter (@ (name ,name)))
;;                (prop context name)
;;                ]
;;               [(c:prop-filter (@ (name ,name)) . ,rest)
;;                ]
;;               [(c:param-filter (@ (name ,name)) . ,rest)]
;;               [(c:is-not-defined)]
;;               [(c:text-match (@ . ,attrs) . ,data)]
;;               [(c:time-range (@ . ,attrs))]))



;; Requests can content-type be both both application/xml and text/xml, server MUST accept both (RFC 4918 8.2)

;; ;; RFC 4918 8.2
;; (catch 'parser-error
;;   (lambda () (xml->sxml body))
;;   (lambda (err input-port . msg)
;;     (define err-msg
;;       (with-output-to-string
;;         (lambda () (for-each display msg))))
;;     (return (build-response code: 400
;;                             headers: ((content-type . (text/plain))))
;;             err-msg)))

;; A caldav server MUST support
;; - RFC4918 (WebDAV) Class 1
;; - RFC3744 WebDAV ACL including additional privilege defined in 6.1
;; - HTTPS
;; - ETags from RFC2616 (http)

;; MKCALENDAR NOT required




;; getcontentlanguage, "dead" property

(declare-method! "PROPFIND" 'PROPFIND)
(declare-method! "PROPPATCH" 'PROPPATCH)
(declare-method! "MKCOL" 'MKCOL)
(declare-method! "COPY" 'COPY)
(declare-method! "MOVE" 'MOVE)
(declare-method! "LOCK" 'LOCK)
(declare-method! "UNLOCK" 'UNLOCK)
(declare-method! "REPORT" 'REPORT)
(declare-method! "MKCALENDAR" 'REPORT)



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
    ;; TODO assert isn't a thing
    ;; (assert (= 1 (string-length str)))
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
                             (make-transcoder (utf-8-codec)))
                            xml->namespaced-sxml))
                       (else (xml-document
                              root: ((xml webdav 'propfind)
                                     ((xml webdav 'allprop))))))))

              (values (build-response
                       code: 207
                       reason-phrase: (http-status-phrase 207)
                       headers: '((content-type . (application/xml))))
                      (apply
                       (xml webdav 'multistatus)
                       (for (href . resource) in requested-resources
                            (apply (xml webdav 'response)
                                   ((xml webdav 'href) (href->string href))
                                   (map propstat->namespaced-sxml
                                        (exec-propfind property-request resource))))))))

        (else (build-response code: 404))))



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


;;; TODO shouldn't root resource actually be used?
(define (run-options _ href request)
  (build-response code: 200
                  headers: `((dav . (1))
                             ;; (DAV . "calendar-access")
                             ;; TODO collecting this set dynamically would be fancy!
                             (allow . (GET HEAD PUT
                                           MKCOL PROPFIND OPTIONS
                                           DELETE
                                           COPY
                                           MOVE
                                           ;; LOCK
                                           ;; UNLOCK
                                           ;; REPORT
                                           )))))

(define (run-get root-resource href request)
  (cond ((lookup-resource root-resource href)
         => (lambda (resource)
              (values (build-response
                       code: 200
                       headers: (filter cdr
                                        `((content-type
                                           . ,(and=> (content-type resource)
                                                     (compose list string->symbol)))
                                          (last-modified . ,(and=> (last-modified resource)
                                                                   (@ (datetime srfi-19) datetime->srfi-19-date)))
                                          (content-language . ,(content-language resource))
                                          (content-length . ,(content-length resource))
                                          (etag . ,(etag resource)))))
                      ;; Content will be filtered out by Guile's
                      ;; webserver for HEAD requests.
                      (content resource))))

        (else (build-response code: 404))))


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
                (set-property! resource ((xml webdav 'getcontenttype) content-type)))))
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



;;; Log tables are tables for easily adding key value data,
;;; and later formatting them.
;;; They in themself do not actually do any logging.

;;; The "global" log table
(define log-table (make-parameter #f))

;;; Initialize the global log table to an empty log table
(define (init-log-table!) (log-table '()))

;;; Takes a list of alternating symbols and values,
;;; Each such pair is added to the log global table
(define (log-table-add! . args)
  (for (key value) in (group args 2)
       (log-table (acons key value (log-table)))))

;;; Get the given key from the global key table
;;; or return dflt (default #f) if not found
(define* (log-table-get key optional: dflt)
  (or (assoc-ref (log-table) key)
      dflt))

;;; Write data from the global log table to current output port.
;;; Each argument should be one of the following types
;;; string? :: printed verbatim
;;; symbol? :: value looked up in the global log table,
;;;            and value printed
;;; pair? :: The car is a symbol to look up per `symbol?'
;;;          The cdr is a procedure for foramtting the given
;;;          value for output
;;; All other types are ignored.
(define (log-table-format . args)
  (for-each (lambda (arg)
              (cond ((string? arg) (display arg))
                    ((symbol? arg) (cond ((log-table-get arg)
                                          => display)))
                    ((pair? arg)   (cond ((log-table-get (car arg))
                                          => (compose display (cdr arg)))))
                    (else #f)))
            args))

;;; Writes a log message to current error port.
;;; This reads values for the log table.
;;;
;;; The following table fields are used
;;; now :: current datetime, as a datetime?
;;; method :: Name of the source method
;;; uri :: URI accessed, an an uri? object
;;; request :: The source request
;;;             If the request-method of the request is
;;;             'COPY or 'MOVE then `headers' is checked for a
;;;             destination header.
;;; headers :: Request headers, see `request'
;;; response-code :: Response code to emit (e.x. 200)
;;; response-phrase :: Phrase belonging to that code (e.x. "OK")
;;; msg :: Optional freetext message
(define (emit-log!)
  ;; (write (log-table) (current-error-port))
  ;; (newline (current-error-port))
  (display
   (with-output-to-string
     (lambda ()
       (log-table-format
        "< " 'method " " (cons 'uri uri->string) "\n"
        "< " 'response-code " " 'response-phrase "\n"
        "< Completed " (cons 'now (lambda (n) (datetime->string n "~H:~M:~S"))) "\n"
        "< Headers:\n"
        (cons 'response (lambda (r)
                          (string-concatenate
                           (for (name . value) in (response-headers r)
                                (format #f "<     ~a: ~s~%" name value)))))
        )
       ;; (log-table-format (cons 'now (lambda (n) (datetime->string n "~H:~M:~S")))
       ;;                   " " 'method " "
       ;;                   (cons 'uri uri->string)
       ;;                   " ")
       ;; (case (request-method (log-table-get 'request))
       ;;   ((COPY MOVE) (log-table-format
       ;;                 (cons 'headers (lambda (h) (and=> (assoc-ref h 'destination) uri->string)))
       ;;                 " "))
       ;;   (else ""))
       ;; Nginx uses
       ;; <ip> - - [<date>] "<request-line>" <request-status> <content-length> "<referer-url>" "<user-agent>"
       ;; (log-table-format 'response-code " "
       ;;                   'response-phrase
       ;;                   " "
       ;;                   (cons 'headers (lambda (h) (assoc-ref h 'x-litmus)))
       ;;                   "\n")

       (cond ((log-table-get 'msg)
              => (lambda (it)
                   (for line in (string-split (string-trim-both it) #\newline)
                        (format #t "<< ~a~%" line)))))

       (cond ((log-table-get 'backtrace)
              => (lambda (it)
                   (for line in (string-split (string-trim-both it) #\newline)
                        (format #t "<<< ~a~%" line)))))

       (newline)))

   (current-error-port))
  )




;; For all headers:
;; `((server ,(format #f "calp/~a" (@ (calp) version)))
;;   (date ,(datetime->string (current-datetime)
;;                            "~a, ~d ~b ~Y ~H:~M:~S GMT"))
;;   (connection keep-alive))

;; Already fixed by server
;;   (content-length ,(format #f (bytevector->length data)))


(define ((webdav-handler root-resource) request request-body)
  (format (current-error-port) "> ~a ~a~%> Headers:~%"
          (request-method request) (uri->string (request-uri request)))
  (for (header . value) in (request-headers request)
       (format (current-error-port) ">     ~a: ~s~%" header value))

  (define href (-> request request-uri uri-path
                   (uri-decode decode-plus-to-space?: #f)
                   string->href))
  (init-log-table!)
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
                                      (format #f "Error in ~a: ~?~%" proc fmt args)
                                      (format #f "~?~%" fmt args))))
                      (log-table-add! 'msg errmsg)
                      (values head errmsg)))
                   (err
                    (let ((errmsg (format #f "General error: ~s~%" err)))
                      (log-table-add! 'msg errmsg)
                      (values (build-response code: 500)
                              errmsg)))))

     ))

  (log-table-add!
   'response response
   'response-code   (response-code response)
   'response-phrase (response-reason-phrase response))

  (emit-log!)

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
                                          (namespaced-sxml->xml body*)
                                          (newline))))
                (else body*))))



;; "/principals/uid/:uid"

#;

(define (make-make-routes)
  (make-routes


   ;; A file extension could be added, but
   ;; text/calendar ⇒ .ics
   ;; application/calendar+xml ⇒ .xcs
   ;; application/calendar+json ⇒ UNKNOWN
   (GET "/caldav/:user/:calendar/:filename" (user calendar filename)
        (define requested-types
          (cond ((assoc-ref r:headers 'accept)
                 => (lambda (accept)
                      (sort* accept <
                             (lambda (type)
                               (or (assoc-ref (cdr type) 'q)
                                   1000)))))
                (else '(text/calendar))))
        (define available-types
          '(text/calendar application/calendar+xml))

        (define content-type (find (lambda (type) (memv type available-types)) requested-types))
        (define serializer
          (case content-type
            ((text/calendar)             ical:serialize)
            ((application/calendar+xml)  xcal:serialize)
            ((application/calendar+sexp) sxcal:serialize)
            (else (return (build-response code: 415)
                          "Bad content type"))))

        (define event
          (copy-as-orphan
           (get-by-uid (get-store-by-name calendar) filename)))

        ;; TODO where is the event split into multiple VEVENT objects in the
        ;; serialized form? Should be in the serializer, right?

        (define component
          (vcalendar prodid: ((@ (calp) prodid))
                     version: "2.0"
                     (list event)))

        (values `((content-type ,content-type))
                (call-with-output-string
                  (lambda (p) (serializer component p)))))

   (PUT "/caldav/:user/:calendar/:filename" (user calendar filename)
        ;; Request Headers:
        ;; If-None-Match
        ;; Content-Type: text/calendar
        ;;               application/calendar+xml

        ;; TODO change -X-HNH to X-HNH-PRIVATE, see RFC4791 5.3.3

        (define component
          (let ((type args (car+cdr (assoc-ref r:headers 'content-type))))
            ;; Valid args: charset component optinfo
            ;; Invalid args: method (see RFC4791 4.1)
            ;; Component is for redundancy?
            ;; optinfo is implementation dependant?
            ;; Charset already handled by HTTP server
           (case type
             ((text/calendar)            (ical:deserialize body))
             ((application/calendar+xml) (xcal:deserialize body))
             (else (return (build-response code: 415)
                           "Can't handle that content type")))))

        (unless (eq? 'VCALENDAR (type component))
          ;; Top level object must be a VCALENDAR
          )

        ;; Must all children be VEVENT?
        (children component)

        ;; All VEVENT component must be the the same event, so they should be merged into a single event
        (define event (handle-events component))

        ;; RFC4791 5.3.2:
        ;; > The URL for each calendar object resource is entirely arbitrary and
        ;; > does not need to bear a specific relationship to the calendar object
        ;; > resource's iCalendar properties or other metadata.  New calendar
        ;; But requiring that UID and filename match makes things easier for us, at least for now
        (unless (string=? filename (prop component 'UID))
          (return (build-response code: 400)
                  "UID and filename must match"))

        (let ((cal (get-calendar-by-name global-event-object calendar)))
          ;; (add-and-save-event global-event-object cal component)

          (reparent! cal event)
          (queue-write (get-store-for-calendar cal) event)

          )

        )
   ))

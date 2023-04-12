(define-module (calp server webdav)
  :use-module ((hnh util) :select (for group -> ->> init+last catch*))
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (ice-9 format)
  :use-module (ice-9 control)
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
  :use-module (calp webdav resource virtual)
  :use-module (calp webdav resource file)
  :use-module (calp webdav property)
  :use-module (calp webdav propfind)
  :use-module (calp webdav proppatch)
  :use-module (oop goops)
  :export (; run-run
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

           root-resource
           webdav-handler
           ))

;; (define* (my-build-response . kvs)
;;   (define dt (datetime->string (current-datetime) "~a, ~d ~b ~Y ~H:~M:~S GMT"))
;;   (define server (format #f "calp/~a" (@ (calp) version)))
;;   (let ((as (kvlist->assq kvs)))
;;     (append kvs
;;             (list
;;              reason-phrase: (http-status-phrase (assq-ref as code:))
;;              headers: (append (or (assq-ref kvs headers:) '())
;;                               (list
;;                                server: server
;;                                date: dt
;;                                connection: 'keep-alive))))))

(define (swap p)
  (xcons (car p) (cdr p)))


(define output-namespaces
  (map (lambda (pair) (call-with-values (lambda () (car+cdr pair))
                   xcons))
       namespaces))

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

;; ;; If a body is sent by the client when not expected, the server MUST repspond
;; ;; with 415 (RFC 4918 8.4)

;; PROPPATCH
;; SHOULD support setting of arbitrary dead properties (RFC4918 9.2)
;; Fruux supports this
;; NOTE this means that user quotas must include dead properties


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



(define (root-element sxml)
  (sxml-match sxml
    [(*TOP* (*PI* . ,args) ,root) root]
    [(*TOP* ,root) root]
    [,root root]))


(define root-resource (make-parameter #f))



(define (parse-dav-line str)
  (map (lambda (item)
         (cond ((string-match "^[0-9]+$" item)
                => (lambda (m) (number->string (match:substring m))))
               ((string-match "^<(.*)>$" item)
                => (lambda (m) (string->uri (match:substring m 1))))
               (else (string->symbol item))))
       (map string-trim-both (string-split str #\,))))

(define (validate-dav-line lst)
  (every (lambda (item)
           (or (and (number? item) (<= 1 item 3))
               (uri? item)
               ;; Possibly check against list of valid tokens
               (symbol? item)))
         lst))

(define (write-dav-line lst port)
  (display
   (string-join (map (lambda (item)
                       (cond ((number? item) (number->string item))
                             ((uri? item) (string-append "<" (uri->string item) ">"))
                             (else (symbol->string item))))
                     lst)
                ", " 'infix)
   port))

(declare-header! "DAV"
  parse-dav-line
  validate-dav-line
  write-dav-line)

(declare-header! "Depth"
  (lambda (str)
    (if (string-ci=? str "Infinity")
        'infinity
        (string->number str)))
  (lambda (value)
    (memv value '(0 1 infinity)))
  (lambda (value port)
    (display value port)))

(declare-header! "Destination"
  string->uri
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



(define (run-propfind href request body)
  (define headers (request-headers request))
  (cond ((lookup-resource (root-resource) href)
         => (lambda (resource)
              (define requested-resources
                (case (or (assoc-ref headers 'depth) 'infinity)
                  ((0) (list (cons href resource)))
                  ((1) (cons (cons href resource)
                             (map (lambda (child)
                                    (cons (append href (list (name child)))
                                          child))
                                  (children resource))))
                  ((infinity) (all-resources-under resource href))))

              ;; Body, if it exists, MUST have be a DAV::propfind object
              (define-values (property-request namespaces*)
                (cond ((string? body)
                       (-> body
                           xml->namespaced-sxml
                           (namespaced-sxml->sxml/namespaces (map swap namespaces))))
                      ((bytevector? body)
                       (-> body
                           (bytevector->string (make-transcoder (utf-8-codec)))
                           xml->namespaced-sxml
                           (namespaced-sxml->sxml/namespaces (map swap namespaces))))
                      (else (values '(d:propfind (d:allprop))
                                    `((d . ,webdav))))))


              (catch 'bad-request
                (lambda ()
                  (values (build-response
                           code: 207
                           reason-phrase: (http-status-phrase 207)
                           headers: '((content-type . (application/xml))))
                          (lambda (port)
                            (namespaced-sxml->xml
                             `(,(xml webdav 'multistatus)
                               ,@(for (href . resource) in requested-resources
                                      `(,(xml webdav 'response)
                                        (,(xml webdav 'href) ,(href->string href))
                                        ,@(map propstat->namespaced-sxml
                                               (parse-propfind (root-element property-request)
                                                               (map swap namespaces*)
                                                               resource)))))
                             namespaces: output-namespaces
                             port: port)
                            (newline port))))
                (lambda (err proc fmt args data)
                  (values (build-response
                           code: 400
                           headers: '((content-type . (text/plain))))
                          (lambda (port)
                            (apply format port fmt args)))))))
        (else (values (build-response code: 404) ""))))



(define (run-proppatch href request body)
  (cond ((lookup-resource (root-resource) href)
         => (lambda (resource)
              ;; Body MUST exist, and be a DAV::propertyupdate element
              (catch 'bad-request
                (lambda ()
                  (values (build-response
                           code: 207
                           reason-phrase: (http-status-phrase 207)
                           headers: '((content-type . (application/xml))))
                          (lambda (port)
                            (define-values (request namespaces*)
                              (cond ((string? body)
                                     (-> body
                                         xml->namespaced-sxml
                                         (namespaced-sxml->sxml/namespaces
                                          (map swap namespaces))))
                                    ((bytevector? body)
                                     (-> body
                                         (bytevector->string (make-transcoder (utf-8-codec)))
                                         xml->namespaced-sxml
                                         (namespaced-sxml->sxml/namespaces
                                          (map swap namespaces))))
                                    (else (throw 'body-required))))

                            (namespaced-sxml->xml
                             `(,(xml webdav 'multistatus)
                               (,(xml webdav 'response)
                                (,(xml webdav 'href) ,(href->string href))
                                ,@(map propstat->namespaced-sxml
                                       (parse-propertyupdate
                                        (root-element request)
                                        (map swap namespaces*)
                                        resource))))
                             port: port))))
                (lambda (err proc fmt args data)
                  (values (build-response
                           code: 400
                           headers: '((content-type . (text/plain))))
                          (lambda (port)
                            (apply format port fmt args)))))))
        (else (values (build-response code: 404) ""))))


(define (run-options href request)
  (values
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
                                        ))))
   ""))

(define (run-get href request mode)
  (cond ((lookup-resource (root-resource) href)
         => (lambda (resource)
              ;; "/calendar/:user/:calendar/:filename"
              ;; headers: `((content-type ,content-type))
              (values (build-response code: 200)
                      (case mode
                        ((HEAD) "")
                        ((GET) (content resource))
                        (else (scm-error 'misc-error "run-get"
                                         "Unknown mode: ~s"
                                         (list mode) #f))))))
        (else (values (build-response code: 404) ""))))

(define (run-put href request request-body)
  (cond ((null? href)
         (values (build-response code: 405 headers: '((content-type . (text/plain))))
                 "Can't PUT on root resource"))
        ((lookup-resource (root-resource) (drop-right href 1))
         => (lambda (parent)
              (cond ((lookup-resource parent (list (last href)))
                     => (lambda (child)
                          (if (is-collection? child)
                              (values (build-response code: 405) "")
                              (begin
                                (set-content! child request-body)
                                (values (build-response code: 204) "")))))
                    (else
                     (add-resource! parent (last href)
                                    request-body)
                     (values (build-response code: 201) "")))))
        ;; No parent collection, fail per [WEBDAV] 9.7.1.
        (else (values (build-response code: 409)))))

(define (run-mkcol href request _)
  ;; TODO href="/"
  (if (assoc-ref (request-headers request) 'content-type)
      (values (build-response code: 415)
              "")
      (let ((path name (init+last href)))
        (cond ((lookup-resource (root-resource) path)
               => (lambda (parent)
                    (catch 'resource-exists
                      (lambda ()
                        (add-collection! parent name)
                        (values (build-response code: 201) ""))
                      (lambda _ (values (build-response code: 405) "")))))
              (else
               (values (build-response code: 409) ""))))))



;;; TODO completely rewrite error handling here
;;; TODO what happens on copy between sub-trees of different types?
;;; Like from a <calendar-resource> tree to a <file-tree>.
(define (run-copy source-href request)
  (define headers (request-headers request))
  (call/ec
   (lambda (return)
    (let* ((depth (or (assoc-ref headers 'depth) 'infinity))
           (destination-uri (assoc-ref headers 'destination))
           (dest-href (-> headers (assoc-ref 'destination)
                          uri-path string->href))
           (overwrite?
            (cond ((assoc 'overwrite headers) => cdr)
                  (else #t))))

      ;; (assert (memv depth '(0 infinity)))
      ;; (unless (string=? (listen-uri) (uri-host destination-uri))
      ;;   (throw 'cross-domain-copy-not-supported))

      (let ((dest-path dest-name (init+last dest-href)))
        (let ((source-resource
               (cond ((lookup-resource (root-resource) source-href) => identity)
                     (else (return (build-response code: 404) ""))))
              (destination-parent-resource
               (cond ((lookup-resource (root-resource) dest-path) => identity)
                     (else (return (build-response
                                    code: 409
                                    reason-phrase: (http-status-phrase 409)
                                    headers: '((content-type . (text/plain))))
                                   "One or more parent components of destination are missing")))))

          (case (copy-to-location! source-resource destination-parent-resource
                                   new-name: dest-name
                                   include-children?: (case depth
                                                        ((0) #f)
                                                        ((infinity) #t)
                                                        (else (throw 'invalid-requeqst)))
                                   overwrite?: overwrite?)
            ((created)
             (values (build-response code: 201) ""))
            ((replaced)
             (values (build-response code: 204) ""))
            ((collision)
             (values (build-response code: 412) "")))))))))


(define (run-delete href request)
  ;; TODO href="/"
  (let ((path name (init+last href)))
    (cond ((lookup-resource (root-resource) path)
           => (lambda (parent)
                (cond ((lookup-resource parent (list name))
                       => (lambda (child)
                            (delete-child! parent child)
                            (values (build-response code: 202)
                                    "")))
                      (else
                       (values (build-response code: 404) "")))))
          (else
           (values (build-response code: 404) "")))))


;;; TODO read spec
(define (run-move href request)
  ;; TODO href="/"
  ;; (format (current-error-port)
  ;;         "MOVE ~s: ~s~%" href request)
  (catch*
   (lambda ()
    (let ((to (-> (request-headers request)
                  (assoc-ref 'destination)
                  uri-path
                  string->href))
          (overwrite? (cond ((assoc 'overwrite request) => cdr)
                            (else #t))))
      (case (move-resource! (root-resource) href to overwrite?)
        ((created)   (values (build-response code: 201) ""))
        ((replaced)  (values (build-response code: 204) ""))
        ((collision) (values (build-response code: 412
                                             headers: '((content-type . (text/plain))))
                             "Something already exists there")))
      ))
   (source-not-found
    (lambda _ (values (build-response code: 404))))
   (target-parent-not-found
    (lambda _ (values (build-response code: 409))))))



;; (define (run-report href request request-body))






(define log-table (make-parameter #f))
(define (init-log-table!) (log-table '()))
(define (log-table-add! . args)
  (for (key value) in (group args 2)
       (log-table (acons key value (log-table)))))
(define* (log-table-get key optional: dflt)
  (or (assoc-ref (log-table) key)
      dflt))

(define (log-table-format . args)
  (for-each (lambda (arg)
              (cond ((string? arg) (display arg))
                    ((symbol? arg) (cond ((log-table-get arg)
                                          => display)))
                    ((pair? arg)   (cond ((log-table-get (car arg))
                                          => (compose display (cdr arg)))))
                    (else #f)))
            args))

(define (emit-log!)
  ;; (write (log-table) (current-error-port))
  ;; (newline (current-error-port))
  (display
   (with-output-to-string
     (lambda ()
       (log-table-format (cons 'now (lambda (n) (datetime->string n "~H:~M:~S")))
                         " " 'method " "
                         (cons 'uri uri->string)
                         " ")
       (case (request-method (log-table-get 'request))
         ((COPY MOVE) (log-table-format
                       (cons 'headers (lambda (h) (and=> (assoc-ref h 'destination) uri->string)))
                       " "))
         (else ""))
       ;; Nginx uses
       ;; <ip> - - [<date>] "<request-line>" <request-status> <content-length> "<referer-url>" "<user-agent>"
       (log-table-format 'response-code " "
                         'response-phrase
                         " "
                         (cons 'headers (lambda (h) (assoc-ref h 'x-litmus)))
                         "\n")

       (cond ((log-table-get 'msg)
              => (lambda (it)
                   (display it)
                   (newline))))))

   (current-error-port))
  )




;; For all headers:
;; `((server ,(format #f "calp/~a" (@ (calp) version)))
;;   (date ,(datetime->string (current-datetime)
;;                            "~a, ~d ~b ~Y ~H:~M:~S GMT"))
;;   (connection keep-alive))

;; Already fixed by server
;;   (content-length ,(format #f (bytevector->length data)))


(define (webdav-handler request request-body)
  (define href (-> request request-uri uri-path string->href))
  (init-log-table!)
  (log-table-add! 'now (current-datetime)
                  'method (request-method request)
                  'uri (request-uri request)
                  'headers (request-headers request)
                  'request request)

  (catch #t
    (lambda ()
      ;; TODO also log result of execution
      (call-with-values
          (lambda ()
            (case (request-method request)
              ((OPTIONS) (run-options href request))

              ((PROPFIND)  (run-propfind  href request request-body))
              ((PROPPATCH) (run-proppatch href request request-body))

              ((GET HEAD) (run-get href request (request-method request)))

              ((PUT) (run-put href request request-body))

              ((DELETE) (run-delete href request))

              ((MKCOL) (run-mkcol href request request-body))

              ((COPY) (run-copy href request))
              ((MOVE) (run-move href request))

              ;; ((REPORT))

              (else (values (build-response code: 400) ""))))
        (lambda (head body)
          (log-table-add!
           'response head
           'response-code (response-code head)
           'response-phrase (response-reason-phrase head))
          (emit-log!)
          (values head body))))

    (case-lambda ((err proc fmt args data)
                  (let ((head (build-response
                               code: 500
                               headers: '((content-type . (text/plain)))))
                        (errmsg (if proc
                                    (format #f "Error in ~a: ~?~%" proc fmt args)
                                    (format #f "~?~%" fmt args))))
                    (log-table-add! 'response head
                                    'response-code 500
                                    'msg errmsg)
                    (emit-log!)
                    (values head errmsg)))
                 (err
                  (let ((errmsg (format #f "General error: ~s~%" err)))
                    (log-table-add! 'response-code 500
                                    'msg errmsg)
                    (emit-log!)
                    (values (build-response code: 500)
                            errmsg))))))



;;; TODO shouldn't this default to #f
(root-resource
 (let ()
   (define root-resource (make <virtual-resource> name: "*root*"))

   (define virtual-resource (make <virtual-resource>
                              name: "virtual"
                              content: (string->bytevector "Hello, World\n" (native-transcoder))))

   (define file-tree (make <file-resource>
                       root: "/home/hugo/tmp"
                       name: "files"))

   (mount-resource! root-resource file-tree)
   (mount-resource! root-resource virtual-resource)
   root-resource))


(define (run-run)
  (unless (root-resource)
    (throw 'misc-error "run-run"
           "root-resource parameter must be set before running"
           (list) #f))
  (run-server webdav-handler
              'http
              `(#:port 8102)))

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

(define-module (web http dav)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-88)
  :use-module (rnrs bytevectors)
  :use-module (rnrs io ports)
  :use-module ((ice-9 binary-ports) :select (call-with-output-bytevector))
  :use-module (web request)
  :use-module (web response)
  :use-module (web client)
  :use-module (web uri)
  :use-module (sxml simple)
  :use-module (sxml xpath)
  :use-module ((hnh util) :select (->))
  :export (caldav
           user-agent dav
           propfind
           get-principal
           get-calendar-home-set
           get-calendar-paths
           get-calendar-name
           )
  )

(define caldav "urn:ietf:params:xml:ns:caldav")
(define user-agent (make-parameter ""))
(user-agent "calp/0.1")

(define-record-type <info>
  (make-info uri-creator password)
  info?
  (uri-creator uri-creator)
  (password info-password)
  )

(define (with-output-to-bytevector thunk)
  (call-with-output-bytevector
   (lambda (port)
     (with-output-to-port port thunk))))

;; Make a webdav HTTP request, body should be a sxml tree without the *TOP* or
;; *PI* element.
(define* (dav uri key: method authorization body (depth 1))
  (define request-body
    (if body
        (with-output-to-bytevector
         (lambda ()
           (sxml->xml
            `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
                    ,body))))
        #f))

  (define headers
    `((user-agent . ,(user-agent))
      (depth . ,(cond (depth number? => number->string)
                      (else depth)))
      ;; (accept . ((*/*)))
      (authorization . ,authorization)
      ,@(if body
            `((content-type . (application/xml (charset . "UTF-8")))
              (content-length . ,(bytevector-length request-body)))
            '())))

  (http-request uri
                method: method
                body: request-body
                headers: headers
                keep-alive?: #t
                decode-body?: #f
                streaming?: #t))

(define* (propfind uri resource key: (depth 1) password)
  (define authorization
    (if password
        `(Basic ,password)
        #f))
  (define-values (response port)
    (dav uri
         method: 'PROPFIND
         authorization: authorization
         depth: depth
         body: `(propfind (@ (xmlns "DAV:")
                             (xmlns:d "DAV:")
                             (xmlns:c ,caldav))
                          (prop (,resource)))))
  (unless (= 207 (response-code response))
    (scm-error 'dav-error "propfind"
               "HTTP error ~a: ~a"
               (list
                (response-code response)
                (response-reason-phrase response))
               (list response)))
  (xml->sxml port
             declare-namespaces?: #t
             trim-whitespace?: #t
             namespaces: `((d . "DAV:")
                           (c . ,caldav))))


;; (define (get-collections)
;;   (-> (propfind "/" 'resourcetype)
;;       ((sxpath '(// (d:response (// d:resourcetype d:collection))
;;                     d:href *text*)))))

;; => ((d:resourcetype (d:collection)))

(define* (get-principal uri key: password)
  (-> (propfind uri 'current-user-principal
                depth: 0
                password: password)
      ((sxpath '(// (d:response (d:href (equal? "/")))
                    //
                    d:prop d:current-user-principal
                    d:href *text*)))
      car))

(define* (get-calendar-home-set principal-uri key: password)
  (-> (propfind principal-uri
                'c:calendar-home-set
                password: password)
      ((sxpath `(// (d:response (d:href
                                 (equal? ,(uri-path principal-uri))))
                    // d:prop c:calendar-home-set
                    d:href *text*
                    )))
      car))

(define* (get-calendar-paths calendar-home-set-uri key: password)
  (-> (propfind calendar-home-set-uri
                'resourcetype
                depth: "infinity"
                password: password)
      ((sxpath '(// (d:response (// d:resourcetype c:calendar))
                    d:href *text*)))))

;; => ("Calendar")
(define* (get-calendar-name calendar-path
                           key: password)
  (-> (propfind calendar-path 'displayname
                depth: 0
                password: password)
      ((sxpath '(// d:response // d:prop d:displayname *text*)))
      car))



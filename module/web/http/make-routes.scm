(define-module (web http make-routes)
  :use-module (hnh util)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 control)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((web query) :select (parse-query))
  :use-module ((web response) :select (build-response))
  :export (parse-endpoint-string
           make-handler
           add-route!
           make-route
           set-request-start-log!
           set-request-end-log!
           set-request-404-handler!
           realize-handler
           )
  )



;; Parses an endpoint description, and returns two values:
;; - a regex string which matches the rule
;; - the list of symbols embedded in the string
;; An endpoint string looks like
;; /calendar/:uid{.*}.ics
;; Where "/calendar/" matches literally
;; followed by something matching ".*"
;; followed by something literally matching ".ics"
;; and '(uid) would be the second return
(define (parse-endpoint-string str)
  (let ((rx (make-regexp ":([^/.]+)(\\{([^}]+)\\})?([.])?")))
    (let loop ((str str)
               (string "")
               (tokens '()))
      (let ((m (regexp-exec rx str 0)))
        (if (not m)
            ;; done
            (values (string-append string str) (reverse tokens))

            (loop (match:suffix m)
                  (string-append string (match:prefix m)
                                 (aif (match:substring m 3)
                                      (string-append "(" it ")")
                                      "([^/.]+)")
                                 ;; period directly following matched variable.
                                 ;; since many variables break on period, we often
                                 ;; want to match a literal period directly after them.
                                 ;; Ideally all periods outside of pattern should be
                                 ;; matched literally, but that's harder to implement.
                                 (regexp-quote
                                  (aif (match:substring m 4)
                                       "." "")))
                  (cons (string->symbol (match:substring m 1))
                        tokens)))))))




;;; TODO replace with better object
((@ (srfi srfi-9) define-record-type) <handler>
 (%make-handler routes start-log end-log 404-handler)
 handler?
 (routes get-routes)       ; hash-table? : method -> listof <endpoint>
 (start-log   get-request-start-log   set-request-start-log!)
 (end-log     get-request-end-log     set-request-end-log!)
 (404-handler get-request-404-handler set-request-404-handler!)
 )

(define (make-handler)
  (%make-handler
   (make-hash-table)
   (lambda _ 'noop) ; request start log
   (lambda _ 'noop) ; request end log
   (lambda _        ; Default 404 handler
     (values (build-response code: 404)
             "404 Not Found"))))

;;; TODO replace with better object
((@ (srfi srfi-9) define-record-type) <endpoint>
 (make-endpoint method path-rx path-parameter-names callback
                )
 endpoint?
 (method get-method)
 (path-rx get-path-rx)
 (path-parameter-names get-path-parameter-names)
 (callback get-callback)
 )



(define (add-route! handler route)
  ;; (typecheck handler handler?)
  ;; (typecheck route endpoint?)
  (hash-set! (get-routes handler)
             (get-method route)
             (append
              (hash-ref (get-routes handler) (get-method route) '())
              (list route))))

;;; Syntax around the endpoint type.
(define-syntax (make-route stx)
  (syntax-case stx ()
    ((_ (method uri param-list handler ...))
     (with-syntax ((r:method   (datum->syntax stx 'r:method))
                   (r:uri      (datum->syntax stx 'r:uri))
                   (r:version  (datum->syntax stx 'r:version))
                   (r:headers  (datum->syntax stx 'r:headers))
                   (r:meta     (datum->syntax stx 'r:meta))
                   (r:scheme   (datum->syntax stx 'r:scheme))
                   (r:userinfo (datum->syntax stx 'r:userinfo))
                   (r:host     (datum->syntax stx 'r:host))
                   (r:port     (datum->syntax stx 'r:port))
                   (r:path     (datum->syntax stx 'r:path))
                   (r:query    (datum->syntax stx 'r:query))
                   (r:fragment (datum->syntax stx 'r:fragment))

                   (return  (datum->syntax stx 'return))
                   (request (datum->syntax stx 'request))
                   (body    (datum->syntax stx 'body))
                   (state   (datum->syntax stx 'state)))

       ;; TODO should we check that no repeat parameters exists in the path?
       (define-values (uri-regex path-parameters)
         (parse-endpoint-string (syntax->datum #'uri)))

       #`(make-endpoint
          (quote method)
          (make-regexp #,(datum->syntax stx (string-append "^" uri-regex "/?$")) regexp/icase)
          (quote #,(datum->syntax stx path-parameters))
          (lambda (r:method r:uri r:version r:headers r:meta
                       r:scheme r:userinfo r:host r:port r:path r:query r:fragment
                       return request body state)
            ;; Leading dummy variable, to ensure we always have at least one keyword argument
            (lambda* (key: #,(datum->syntax stx (gensym "unused")) #,@#'param-list allow-other-keys:)
              handler ...)))))))



;;; Given a handler object, create the true function which may be passed to (web server)
;;; realize-handler :: (request, body) -> (values response response-body [state])
(define (realize-handler handler)
  ;; (typecheck handler handler?)
  (lambda* (request request-body optional: state)

    (let ((r:method  ((@ (web request) request-method)  request))
          (r:uri     ((@ (web request) request-uri)     request))
          (r:version ((@ (web request) request-version) request))
          (r:headers ((@ (web request) request-headers) request))
          (r:meta    ((@ (web request) request-meta)    request)))
      (let ((r:scheme   ((@ (web uri) uri-scheme)   r:uri))
            (r:userinfo ((@ (web uri) uri-userinfo) r:uri))
            ;; uri-{host,port} is (probably) not set when we are a server,
            ;; fetch them from the request instead
            (r:host     (or ((@ (web uri) uri-host)     r:uri)
                            (and=> ((@ (web request) request-host) request) car)))
            (r:port     (or ((@ (web uri) uri-port)     r:uri)
                            (and=> ((@ (web request) request-host) request) cdr)))
            (r:path     ((@ (web uri) uri-path)     r:uri))
            (r:query    ((@ (web uri) uri-query)    r:uri))
            (r:fragment ((@ (web uri) uri-fragment) r:uri)))

        ;; Information about the request, passed to all callbacks registered on the handler
        (define common-callback-args
          (list
           request: request request-body: request-body

           method: r:method uri: r:uri version: r:version headers: r:headers
           meta: r:version scheme: r:scheme userinfo: r:userinfo host: r:host
           port: r:host path: r:path query: r:query))

        (apply (get-request-start-log handler) common-callback-args)

        (define-values (headers body new-state)
          (call-with-values
              (lambda ()
                (call/ec
                 (lambda (return)
                   (let loop ((handlers (or (hash-ref (get-routes handler) r:method) '())))
                     (cond ((null? handlers)
                            (call-with-values
                                (lambda () (apply (get-request-404-handler handler)
                                             common-callback-args))
                              return))

                           ((regexp-exec (get-path-rx (car handlers))
                                         r:path)
                            => (lambda (m)
                                 (apply ((get-callback (car handlers))
                                         r:method r:uri r:version r:headers r:meta
                                         r:scheme r:userinfo r:host r:port r:path r:query r:fragment
                                         return request request-body state)

                                        ;; Query parameters ALWAYS before path parameters, since
                                        ;; lambda* takes the last given value as the one to use
                                        ;; See the guile documentation,
                                        ;; header "lambda* and define*" (§6.7.4.1 as of Guile 3.0.10).
                                        (append
                                         (parse-query r:query)
                                         (concatenate
                                          (map list
                                               (map symbol->keyword (get-path-parameter-names (car handlers)))
                                               (map (lambda (i) (match:substring m i))
                                                    (cdr (iota (match:count m))))))))))

                           (else (loop (cdr handlers))))))))
            (case-lambda ((headers body new-state) (values headers body new-state))
                         ((headers body)           (values headers body state))
                         ((headers)                (values headers "" state)))))

        (apply (get-request-end-log handler)
               response-headers: headers response-body: body
               common-callback-args)

        (values headers body new-state)))))

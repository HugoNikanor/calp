(define-module (web http make-routes)
  :export (make-routes)
  :use-module (calp util)
  :use-module (ice-9 regex)
  :use-module (srfi srfi-1)
  :use-module (web response)
  :use-module (web uri))



(define-public (parse-endpoint-string str)
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

(define (generate-case defn)
  (let* (((method uri param-list . body) defn)
         (regex tokens (parse-endpoint-string uri))
         (diff intersect (lset-diff+intersection eq? param-list tokens)))
    `((and (eq? r:method (quote ,method))
           (regexp-exec (make-regexp ,(string-append "^" regex "/?$") regexp/icase)
                        r:path))
      => (lambda (match-object)
           ;; (assert
           ;;  (= (1- (match:count match-object))
           ;;     (length intersect)))

           ;; Those parameters which were present in the template uri
           ((lambda ,intersect
               ;; Those that only are in the query string
               (lambda* (,@(unless (null? diff) `(#:key ,@diff #:allow-other-keys))
                         #:rest rest)
                 ,@body))
            ,@(unless (null? intersect)
                (map (lambda (i)
                       `(match:substring match-object ,i))
                     (cdr (iota (1+ (length intersect)))))))))))

(define-macro (make-routes . routes)

  `(lambda* (request body #:optional state)
     ;; (format (current-error-port) "~a~%"  request)
     ;; ALl these bindings generate compile time warnings since the expansion
     ;; of the macro might not use them. This isn't really a problem.
     (let ((r:method  ((@ (web request) request-method)  request))
           (r:uri     ((@ (web request) request-uri)     request))
           (r:version ((@ (web request) request-version) request))
           (r:headers ((@ (web request) request-headers) request))
           (r:meta    ((@ (web request) request-meta)    request))
           (r:port    ((@ (web request) request-port)    request)))
       (let ((r:scheme   ((@ (web uri) uri-scheme)   r:uri))
             (r:userinfo ((@ (web uri) uri-userinfo) r:uri))
             ;; TODO can sometimes be a pair of host and port
             ;; '("localhost" . 8080). It shouldn't...
             (r:host     (or ((@ (web uri) uri-host) r:uri)
                             ((@ (web request) request-host)
                              request)))
             (r:port     (or ((@ (web uri) uri-port) r:uri)
                             ((@ (web request) request-port)
                              request)))
             (r:path     ((@ (web uri) uri-path)     r:uri))
             (r:query    ((@ (web uri) uri-query)    r:uri))
             (r:fragment ((@ (web uri) uri-fragment) r:uri)))
         ;; TODO propper logging
         (display (format #f "[~a] ~a ~a/~a?~a~%"
                          (datetime->string (current-datetime))
                          r:method r:host r:path (or r:query ""))
                  (current-error-port))
         (call-with-values
             (lambda ()
               ((@ (ice-9 control) call/ec)
                (lambda (return)
                  (apply
                   (cond ,@(map generate-case routes)
                         (else (lambda* _ (return (build-response #:code 404)
                                                  "404 Not Fonud"))))
                   (append
                    ((@ (web query) parse-query) r:query)

                    (let ((content-type (assoc-ref r:headers 'content-type)))
                      (when content-type
                        (let ((type (car content-type))
                              (args (cdr content-type)))
                          (when (eq? type 'application/x-www-form-urlencoded)
                            (let ((encoding (or (assoc-ref args 'encoding) "UTF-8")))
                              ((@ (web query) parse-query)
                               ((@ (ice-9 iconv) bytevector->string)
                                body encoding)
                               encoding)))))))))))
           (case-lambda ((headers body new-state) (values headers body new-state))
                        ((headers body) (values headers body state))
                        ((headers) (values headers "" state))))))))

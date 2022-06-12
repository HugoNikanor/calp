(define-module (web http make-routes)
  :export (make-routes)
  :use-module (hnh util)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :use-module (ice-9 curried-definitions)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  )



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


(define ((generate-case regex-table) defn)
  (match defn
    ((method uri param-list body ...)
     (let* ((_ tokens (parse-endpoint-string uri))
               (diff intersect (lset-diff+intersection eq? param-list tokens)))
          `((and (eq? r:method (quote ,method))
                 (regexp-exec ,(car (assoc-ref regex-table uri)) r:path))
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
                             `((@ (ice-9 regex) match:substring) match-object ,i))
                           (cdr (iota (1+ (length intersect)))))))))))))

(define-macro (make-routes . routes)
  ;; Ensures that all regexes are only compiled once.
  (define routes-regexes
   (map (lambda (uri)
          (define-values (regex _) (parse-endpoint-string uri))
          (list uri (gensym) `(make-regexp ,(string-append "^" regex "/?$") regexp/icase)))
        (map cadr routes)))

  `(let ,(map cdr routes-regexes)
       (lambda* (request body #:optional state)
         ;; (format (current-error-port) "~a~%"  request)
         ;; All these bindings generate compile time warnings since the expansion
         ;; of the macro might not use them. This isn't really a problem.
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
             ;; TODO propper logging
             (display (format #f "[~a] ~a ~a:~a~a?~a~%"
                              (datetime->string (current-datetime))
                              r:method r:host r:port r:path (or r:query ""))
                      (current-error-port))
             (call-with-values
                 (lambda ()
                   ((@ (ice-9 control) call/ec)
                    (lambda (return)
                      (apply
                       (cond ,@(map (generate-case routes-regexes) routes)
                             (else (lambda* _ (return ((@ (web response) build-response) code: 404)
                                                      "404 Not Fonud"))))
                       (append
                        ((@ (web query) parse-query) r:query)

                        ;; TODO what's happening here?
                        (let ((content-type (assoc-ref r:headers 'content-type)))
                          ((@ (hnh util) when) content-type
                           (let ((type (car content-type))
                                 (args (cdr content-type)))
                             ((@ (hnh util) when)
                              (eq? type 'application/x-www-form-urlencoded)
                              (let ((encoding (or (assoc-ref args 'encoding) "UTF-8")))
                                ((@ (web query) parse-query)
                                 ((@ (ice-9 iconv) bytevector->string)
                                  body encoding)
                                 encoding)))))))))))
               (case-lambda ((headers body new-state) (values headers body new-state))
                            ((headers body) (values headers body state))
                            ((headers) (values headers "" state)))))))))

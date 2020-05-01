(define-module (server macro)
  :export (make-routes)
  :use-module (util)
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
           ((lambda ,intersect
               (lambda* (,@(unless (null? diff) `(#:key ,@diff #:allow-other-keys))
                         #:rest rest)
                 ,@body))
            ,@(unless (null? intersect)
                (map (lambda (i)
                       `(match:substring match-object ,i))
                     (cdr (iota (1+ (length intersect)))))))))))

(define-macro (make-routes . routes)

  `(lambda* (request body #:optional state)
     ;; ALl these bindings generate compile time warnings since the expansion
     ;; of the macro might not use them. This isn't really a problem.
     (let ((r:method  (request-method  request))
           (r:uri     (request-uri     request))
           (r:version (request-version request))
           (r:headers (request-headers request))
           (r:meta    (request-meta    request))
           (r:port    (request-port    request)))
       (let ((r:scheme   (uri-scheme   r:uri))
             (r:userinfo (uri-userinfo r:uri))
             (r:host     (uri-host     r:uri))
             (r:port     (uri-port     r:uri))
             (r:path     (uri-path     r:uri))
             (r:query    (uri-query    r:uri))
             (r:fragment (uri-fragment r:uri)))


         (call-with-values
             (lambda ()
              (call/ec (lambda (return)
                         (apply
                          (cond ,@(map generate-case routes)
                                (else (lambda* _ (return (build-response #:code 404)
                                                         "404 Not Fonud"))))
                          (append
                           (parse-query r:query)

                           (when (memv 'application/x-www-form-urlencoded
                                    (or (assoc-ref r:headers 'content-type) '()))
                             (parse-query (uri-decode (bytevector->string body "UTF-8")))))))))
           (lambda* (a b #:optional new-state)
             (values a b (or new-state state))))))))

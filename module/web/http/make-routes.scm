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
  :use-module ((ice-9 iconv) :select (bytevector->string))
  :export (parse-endpoint-string
           make-routes)
  )



;; Parses an endpoint description, and returns two values:
;; - a regex string which matches the rule
;; - the list of symbols embedded int the string
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

(define ((generate-case regexes r:method r:path) stx)
  (syntax-case stx ()
    ((method uri param-list body ...)
     (let* ((regex tokens (parse-endpoint-string (syntax->datum #'uri)))
            (diff intersect (lset-diff+intersection eq? (syntax->datum #'param-list)
                                                    tokens))
            (argument-list (if (null? diff)
                               #'() #`(key: #,@(map (lambda (x) (datum->syntax stx x)) diff)
                                            allow-other-keys: rest: rest)))
            (intersect-list (map (lambda (x) (datum->syntax stx x)) intersect))
            (rx-var (list-ref (assoc regex regexes) 1)))
       #`((and (eq? #,r:method (quote method))
               (regexp-exec #,rx-var #,r:path))
          => (lambda (match-object)
               ;; Those parameters which were present in the template uri
               ((lambda #,intersect-list
                  ;; Those that only are in the query string
                  (lambda* #,argument-list body ...))
                #,@(unless (null? intersect)
                     (map (lambda (i) #`(match:substring match-object #,i))
                          (cdr (iota (1+ (length intersect)))))))))))))



(define-syntax (make-routes stx)
  (syntax-case stx ()
    ((_ options-and-routes ...)
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
                   (state   (datum->syntax stx 'state))
                   )

       (define-values (options routes)
         (let loop ((options '()) (items #'(options-and-routes ...)))
           (when (null? items)
             (scm-error 'misc-error "make-routes"
                        "Needs at least one route" '() #f))
           ;; (format #t "options: ~s, items: ~s~%" options items)
           (let ((kv (syntax->datum (car items))))
             (if (keyword? kv)
                 (loop (cons (cons kv (cadr items))
                             options)
                       (cddr items))
                 (values (reverse options) items)))))

       ;; Ensures that all regexes are only compiled once.
       ;; Given (GET "/today/" (view date) body ...)
       ;; returns ("/today/" #'*random-symbol* #'(make-regexp "^/today//?$" regexp/icase))
       (define routes-regexes
         (map (lambda (stx-1)
                (syntax-case stx-1 ()
                  ((%fst uri %rest ...)
                   (let ((regex _ (parse-endpoint-string (syntax->datum #'uri))))
                     (list regex (datum->syntax stx (gensym "rx-"))
                           #`(make-regexp #,(string-append "^" regex "/?$") regexp/icase))))))
              routes))

       #`(let #,(map cdr routes-regexes)
           (lambda* (request body optional: state)
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
                     (call/ec (lambda (return)
                                (apply
                                 (with-throw-handler #t
                                   (lambda ()
                                     (cond #,@(map (generate-case routes-regexes #'r:method #'r:path) routes)
                                           (else (lambda* _ (return (build-response code: 404)
                                                                    "404 Not Fonud")))))
                                   #,(assoc-ref options with-throw-handler:))
                                 (append
                                  (parse-query r:query)

                                  ;; When content-type is application/x-www-form-urlencoded,
                                  ;; decode them, and add it to the argument list
                                  (let ((content-type (assoc-ref r:headers 'content-type)))
                                    (when content-type
                                      (let ((type args (car+cdr content-type)))
                                        (when (eq? type 'application/x-www-form-urlencoded)
                                          (let ((encoding (or (assoc-ref args 'encoding) "UTF-8")))
                                            (parse-query (bytevector->string body encoding)
                                                         encoding))))))))))

                   (case-lambda ((headers body new-state) (values headers body new-state))
                                ((headers body)           (values headers body state))
                                ((headers)                (values headers "" state))))))))))))

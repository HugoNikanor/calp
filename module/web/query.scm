;; TODO shouldn't this be (web uri query), or are query strings
;; applicable in other contexts?
(define-module (web query)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (web uri)
  :export (parse-query
           encode-query-parameters))

(define* (parse-query query-string
                      optional: (encoding "UTF-8")
                      key: (decode-plus-to-space? #t))
  (define (decode v)
    (uri-decode v encoding: encoding decode-plus-to-space?: decode-plus-to-space?))
  (unless (or (not query-string) (string-null? query-string))
    (fold (lambda (str list)
            ;; only split on the first equal.
            ;; Does HTTP allow multiple equal signs in a data field?
            (let ((key val
                      (cond ((string-index str #\=)
                             => (lambda (idx)
                                  (values (decode (substring str 0 idx))
                                          (decode (substring str (1+ idx))))))
                            (else (let ((v (decode str)))
                                    (values v v))))))
              (cons* (-> key string->symbol symbol->keyword) val list)))
          '() (string-split query-string #\&))))


(define (encode-query-parameters parameters)
  (string-join
   (map (lambda (p)
          (format #f "~a=~a"
                  (uri-encode (with-output-to-string (lambda () (display (car p)))))
                  (uri-encode (with-output-to-string (lambda () (display (cdr p)))))))
        parameters)
   "&"))

;; TODO shouldn't this be (web uri query), or are query strings
;; applicable in other contexts?
(define-module (web query)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (web uri)
  :export (parse-query
           encode-query-parameters))

(define* (parse-query query-string optional: (encoding "UTF-8"))
  (unless (or (not query-string) (string-null? query-string))
    (fold (lambda (str list)
            ;; only split on the first equal.
            ;; Does HTTP allow multiple equal signs in a data field?
            (let ((key val
                      (cond ((string-index str #\=)
                             => (lambda (idx)
                                  (values (uri-decode (substring str 0 idx)    encoding: encoding)
                                          (uri-decode (substring str (1+ idx)) encoding: encoding))))
                            (else (let ((v (uri-decode str encoding: encoding)))
                                    (values v v))))))
              (cons* (-> key string->symbol symbol->keyword) val list)))
          '() (string-split query-string #\&))))


;; TODO why this format for values?
;; TODO why aren't we encoding the keys?
(define (encode-query-parameters parameters)
  (string-join
   (map (lambda (p)
          (format #f "~a=~a"
                  (car p)
                  (uri-encode (with-output-to-string (lambda () (write (cdr p)))))))
        parameters)
   "&"))

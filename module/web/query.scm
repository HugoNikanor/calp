(define-module (web query)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (web uri))

(define*-public (parse-query query-string optional: (encoding "UTF-8"))
  (unless (or (not query-string) (string-null? query-string))
    (fold (lambda (str list)
            ;; only split on the first equal.
            ;; Does HTTP allow multiple equal signs in a data field?
            (let* ((key val
                       (cond ((string-index str #\=)
                              => (lambda (idx)
                                   (values (uri-decode (substring str 0 idx)    encoding: encoding)
                                           (uri-decode (substring str (1+ idx)) encoding: encoding))))
                             (else (let ((v (uri-decode str encoding: encoding)))
                                     (values v v))))))
              (cons* (-> key string->symbol symbol->keyword) val list)))
          '() (string-split query-string #\&))))

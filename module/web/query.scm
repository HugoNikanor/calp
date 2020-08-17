(define-module (web query)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (web uri))

(define*-public (parse-query query-string optional: (encoding "UTF-8"))
  (unless (or (not query-string) (string-null? query-string))
    (fold (lambda (str list)
            ;; only split on the first equal.
            ;; Does HTTP allow multiple equal signs in a data field?
            ;; NOTE that this fails if str lacks an equal sign.
            (define idx (string-index str #\=))
            (define key (uri-decode (substring str 0 idx) encoding: encoding))
            (define val (uri-decode (substring str (1+ idx)) encoding: encoding))
            (cons* (-> key string->symbol symbol->keyword) val list))
          '() (string-split query-string #\&))))

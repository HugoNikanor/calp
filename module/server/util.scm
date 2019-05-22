(define-module (server util)
  :use-module (util)
  :use-module (srfi srfi-1))

(define-public (parse-query query)
  (when query
    (fold (lambda (str list)
            (let* (((k v) (string-split str #\=)))
              (cons* (-> k string->symbol symbol->keyword) v list)))
          '() (string-split query #\&))))

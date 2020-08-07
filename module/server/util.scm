(define-module (server util)
  :use-module (util)
  :use-module (srfi srfi-1))


(define-public (parse-query query-string)
  (unless (string-null? query-string)
   (fold (lambda (str list)
           ;; only split on the first equal.
           ;; Does HTTP allow multiple equal signs in a data field?
           ;; NOTE that this fails if str lacks an equal sign.
           (define idx (string-index str #\=))
           (define key (substring str 0 idx))
           (define val (substring str (1+ idx)))
           (cons* (-> key string->symbol symbol->keyword) val list))
         '() (string-split query-string #\&))))

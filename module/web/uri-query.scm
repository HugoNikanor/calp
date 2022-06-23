(define-module (web uri-query)
  :use-module ((hnh util) :select (->quoted-string))
  :use-module ((web uri) :select (uri-encode))
  :export (encode-query-parameters)
  )

;; TODO why this format for values?
;; TODO why aren't we encoding the keys?
;; TODO why isn't this in the same module as `parse-query'?
;; TODO why isn't this on the same format as `parse-query'?

(define (encode-query-parameters parameters)
  (string-join
   (map (lambda (p)
          (format #f "~a=~a"
                  (car p)
                  (uri-encode (->quoted-string (cdr p)))))
        parameters)
   "&"))


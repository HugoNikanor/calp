(define-module (web uri-query)
  :use-module ((calp util) :select (->quoted-string))
  :use-module ((web uri) :select (uri-encode))
  )


(define-public (encode-query-parameters parameters)
  (string-join
   (map (lambda (p)
          (format #f "~a=~a"
                  (car p)
                  (uri-encode (->quoted-string (cdr p)))))
        parameters)
   "&"))


(define-module (hnh util assert)
  :use-module (rnrs base)
  :export (assert*)
  )

(define-syntax assert*
  (syntax-rules ()
    ((_ assertion)
     (assert assertion))))

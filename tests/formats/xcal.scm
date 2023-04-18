(define-module (xcal)
  :use-module (srfi srfi-88)
  :use-module (hnh test xmllint)
  :use-module (hnh util path)
  :use-module ((rnrs io ports) :select (get-string-all))
  :use-module ((vcomponent formats xcal) :prefix #{xcs:}#)
  :use-module ((calp namespaces) :select (xcal))
  :export (sanitize-string
           serialize
           deserialize
           component-str))

(define (sanitize-string str)
  (xmllint str))

(define serialize
  (lambda (component port)
    (xcs:serialize
     component port namespaces: `((,xcal . c))
     )))

(define deserialize xcs:deserialize)

(define component-str
  (call-with-input-file (path-append (getenv "here") "event.xcs")
    get-string-all))

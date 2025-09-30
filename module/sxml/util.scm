(define-module (sxml util)
  :use-module (srfi srfi-71)
  :use-module ((hnh util) :select (init+last))
  :use-module (ice-9 match)
  :export (modify-root-element add-attributes))

(define (modify-root-element tree modifier)
  (match tree
    (('*TOP* rest ...)
     (let ((init last (init+last rest)))
       `(*TOP* ,@init ,(modifier last))))
    (root (modifier root))))

(define (add-attributes element added-attributes)
  (match element
    ((el ('@ . attributes) . children)
     `(,el (@ ,@attributes ,@added-attributes)
           ,@children))
    ((el . children)
     `(,el (@ ,@added-attributes)
           ,@children))))

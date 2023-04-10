(define-module (sxml util)
  :use-module (ice-9 match)
  :export (get-root-element add-attributes))

(define (get-root-element tree)
  (match tree
    (('*TOP* ('*PI* 'xml body) (root . children))
     (lambda (modifier) `(*TOP* (*PI* xml ,body)
                           ,(modifier `(,root ,@children)))))
    (('*TOP* (root . children))
     (lambda (modifier) `(*TOP* ,(modifier `(,root ,@children)))))
    ((root . children)
     (lambda (modifier) `(*TOP* ,(modifier `(,root ,@children)))))))

(define (add-attributes element added-attributes)
  (match element
    ((el ('@ . attributes) . children)
     `(,el (@ ,@attributes ,@added-attributes)
           ,@children))
    ((el . children)
     `(,el (@ ,@added-attributes)
           ,@children))))

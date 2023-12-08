(define-module (sxml namespaced util)
  :use-module (sxml namespaced)
  :use-module (srfi srfi-1)
  :use-module ((ice-9 control) :select (call/ec))
  :use-module (hnh util type)
  :export (xml-element-hash-key
           find-child
           element-matches?
           root-element
           ))

(define (xml-element-hash-key tag)
  "Returns a value suitable as a key to hash-ref (and family)"
  (cons (xml-element-namespace tag)
        (xml-element-tagname tag)))

(define (find-child target list)
  (typecheck target xml-element?)
  (typecheck list (list-of (or xml-element? string?)))
  (define target* (xml-element-hash-key target))
  (find (lambda (x) (and (xml-element? x)
                    (equal? target* (xml-element-hash-key x))))
        list))


(define (element-matches? target-el tree)
  (and (not (null? tree))
       (equal?
        (xml-element-hash-key target-el)
        (xml-element-hash-key (car tree)))))

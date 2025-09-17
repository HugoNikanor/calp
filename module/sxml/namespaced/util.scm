(define-module (sxml namespaced util)
  :use-module (sxml namespaced)
  :use-module (srfi srfi-1)
  :use-module ((ice-9 control) :select (call/ec))
  :use-module (hnh util type)
  :export (xml-element-hash-key
           find-child
           element-matches?
           root-element
           tag-matches?
           ))

(define (xml-element-hash-key tag)
  "Returns a value suitable as a key to hash-ref (and family)"
  (string->symbol
   (format #f "~a:~a"
           (and=> (xml-element-namespace tag) symbol->string)
           (xml-element-tagname tag))))

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


(define* (tag-matches? xml-element tagname optional: namespace)
  (typecheck xml-element xml-element?)
  (typecheck tagname symbol?)
  (typecheck namespace (or symbol? false?))
  (and (eqv? tagname (xml-element-tagname xml-element))
       (eqv? namespace (xml-element-namespace xml-element))))

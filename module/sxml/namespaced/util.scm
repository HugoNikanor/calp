(define-module (sxml namespaced util)
  :use-module (sxml namespaced)
  :use-module (srfi srfi-1)
  :use-module ((ice-9 control) :select (call/ec))
  :export (xml-element-hash-key
           find-element
           element-matches?
           root-element
           ))

(define (xml-element-hash-key tag)
  "Returns a value suitable as a key to hash-ref (and family)"
  (cons (xml-element-namespace tag)
        (xml-element-tagname tag)))

(define (find-element target list)
  (define target* (xml-element-hash-key target))
  (find (lambda (x) (and (list? x)
                    (not (null? x))
                    (xml-element? (car x))
                    (equal? target* (xml-element-hash-key (car x)))))
        list))


(define (element-matches? target-el tree)
  (and (not (null? tree))
       (equal?
        (xml-element-hash-key target-el)
        (xml-element-hash-key (car tree)))))


(define (root-element tree)
  (cond ((and (eq? '*TOP* (car tree))
              (pi-element? (cadr tree)))
         (caddr tree))
        ((eq? '*TOP* (car tree))
         (cadr tree))
        (else tree)))

(define-module (vcomponent base)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module (vcomponent primitive)
  :use-module (ice-9 hash-table)
  :use-module ((ice-9 optargs) :select (define*-public))
  :re-export (add-child!))

;; vline → value
(define-public value
  (make-procedure-with-setter
   (lambda (vline) (struct-ref vline 0))
   (lambda (vline value) (struct-set! vline 0 value))))

;; vcomponent x (or str symb) → vline
(define-public (attr* component attr)
  (hash-ref (struct-ref component 3)
            (as-string attr)))

;; vcomponent x (or str symb) → value
(define (get-attr component attr)
  (and=> (attr* component attr)
         value))

(define (set-attr! component attr value)
  (aif (attr* component attr)
       (struct-set! it 0 value)
       (hash-set! (struct-ref component 3)
                  (as-string attr)
                  (make-vline value))))

(define-public attr
  (make-procedure-with-setter
   get-attr
   set-attr!))


(define-public prop
  (make-procedure-with-setter
   (lambda (attr-obj prop-key)
     (hash-ref (struct-ref attr-obj 1) prop-key))
   (lambda (attr-obj prop-key val)
     (hash-set! (struct-ref attr-obj 1) prop-key val))))

;; Returns the properties of attribute as an assoc list.
;; @code{(map car <>)} leads to available properties.
(define-public (properties attrptr)
  (hash-map->list cons (struct-ref attrptr 1)))

(define-public type (make-procedure-with-setter
                     (lambda (c) (struct-ref c 0))
                     (lambda (c v) struct-set! c 0 v)
                    ))

(define-public (parent c) (struct-ref c 2))

(define-public (attributes component)
  (hash-map->list cons (struct-ref component 3)))

(define*-public (children component)
  (struct-ref component 1))

(define (copy-vline vline)
  (make-struct/no-tail (struct-vtable vline)
                       (struct-ref vline 0)
                       ;; TODO deep-copy on properties?
                       (struct-ref vline 1)))

(define-public (copy-vcomponent component)
  (make-struct/no-tail (struct-vtable component)
                       (struct-ref component 0)
                       (struct-ref component 1)
                       (struct-ref component 2)
                       (alist->hash-table
                        (hash-map->list (lambda (key value) (cons key (copy-vline value)))
                                        (struct-ref component 3)))))

(define-public (extract field)
  (lambda (e) (attr e field)))

(define-public (extract* field)
  (lambda (e) (attr* e field)))

(define-public (key=? k1 k2)
  (eq? (as-symb k1)
       (as-symb k2)))

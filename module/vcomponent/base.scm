(define-module (vcomponent base)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module (vcomponent primitive)
  :use-module ((ice-9 optargs) :select (define*-public)))

(define (get-attr component attr)
  (and=> (hash-ref (struct-ref component 3)
                   (as-string attr))
         (lambda (l) (struct-ref l 0)))
  #;
  (%vcomponent-get-attribute
   component
   (as-string attr)))

(define (set-attr! component attr value)
  'noop
  #;
  (set! (car (get-attr component (as-string attr)))
        value))

;; (define-public value caar)

;; (define-public (values-left-count attr-list)
;;   (length (take-while identity attr-list)))

;; (define-public (value-count attr-list)
;;   (length (take-while identity (cdr (drop-while identity attr-list)))))

(define-public attr* get-attr)

;; (define (get-first c a)
;;   (and=> (car (get-attr c a)) car))

;; (define (set-first! c a v)
;;   (and=> (car (get-attr c a))
;;          (lambda (f) (set! (car f) v))))

(define-public attr
  (make-procedure-with-setter
;    get-first set-first!
   get-attr
   set-attr!
   ))


(define-public prop
  (make-procedure-with-setter
   (lambda (attr-obj prop-key)
     (hashq-ref (cdar attr-obj) prop-key))
   (lambda (attr-obj prop-key val)
     (hashq-set! (cdar attr-obj) prop-key val))))

;; Returns the properties of attribute as an assoc list.
;; @code{(map car <>)} leads to available properties.
(define-public (properties attrptr)
  (hash-map->list cons (cdar attrptr)))

(define-public type (make-procedure-with-setter
                     (lambda (c) (struct-ref c 0))
                     (lambda (c v) struct-set! c 0 v)
                    ))
(define-public (parent c) (struct-ref c 2))
(define-public push-child! add-child!)
(define-public (attributes component) '("noop")
  #; (map string->symbol (%vcomponent-attribute-list component))
  )

(define*-public (children component #:optional only-type)
  (let ((childs (struct-ref component 1)))
    (if only-type
        (filter (lambda (e) (eq? only-type (type e))) childs)
        childs)))

;; (define-public copy-vcomponent %vcomponent-shallow-copy)

;; (define-public filter-children! %vcomponent-filter-children!)

(define-public (extract field)
  (lambda (e) (attr e field)))

(define-public (extract* field)
  (lambda (e) (attr* e field)))

(define-public (key=? k1 k2)
  (eq? (as-symb k1)
       (as-symb k2)))

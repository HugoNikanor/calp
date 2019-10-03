(define-module (vcomponent base)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module (vcomponent primitive)
  :use-module ((ice-9 optargs) :select (define*-public)))

;; (define og-struct-ref struct-ref)
;; (define (struct-ref struct field)
;;   (format #t "struct = ~a, field = ~a~%" struct field)
;;   (og-struct-ref struct field))

(use-modules (system vm trap-state))

(install-trap-handler! (lambda args (format #t "args = ~a~%" args)))

(add-trace-at-procedure-call! struct-ref)
(add-trap-at-procedure-call! struct-ref)

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
  (format #t "attr = ~a~%" attr)
  (aif (attr* component attr)
       (begin (format #t "Existed~%") (struct-set! it 0 value))
       (begin (format #t "Creating, component = ~a, attr = ~a, value = ~a~%" component attr value)
              (format #t "map = ~a~%" (struct-ref component 3))
              (let ((return (hash-set! (struct-ref component 3)
                                (as-string attr)
                                (make-vline value))))

                (format #t "Return = ~a~%" return)
                return
                )

              )))

;; (define-public (values-left-count attr-list)
;;   (length (take-while identity attr-list)))

;; (define-public (value-count attr-list)
;;   (length (take-while identity (cdr (drop-while identity attr-list)))))

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
     (hashq-ref (struct-ref attr-obj 1) prop-key))
   (lambda (attr-obj prop-key val)
     (hashq-set! (struct-ref attr-obj 1) prop-key val))))

;; Returns the properties of attribute as an assoc list.
;; @code{(map car <>)} leads to available properties.
(define-public (properties attrptr)
  (hash-map->list cons (struct-ref attrptr 1)))

(define-public type (make-procedure-with-setter
                     (lambda (c) (struct-ref c 0))
                     (lambda (c v) struct-set! c 0 v)
                    ))

(define-public (parent c) (struct-ref c 2))
(define-public push-child! add-child!)
(define-public (attributes component)
  (hash-map->list cons (struct-ref component 3))
  #; (map string->symbol (%vcomponent-attribute-list component))
  )

(define*-public (children component)
  (struct-ref component 1))

(define-public (copy-vcomponent component)
  (make-struct/no-tail (struct-vtable component)
                       (struct-ref component 0)
                       (struct-ref component 1)
                       (struct-ref component 2)
                       (struct-ref component 3)))

;; (define-public filter-children! %vcomponent-filter-children!)

(define-public (extract field)
  (lambda (e) (attr e field)))

(define-public (extract* field)
  (lambda (e) (attr* e field)))

(define-public (key=? k1 k2)
  (eq? (as-symb k1)
       (as-symb k2)))

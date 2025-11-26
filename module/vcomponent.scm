(define-module (vcomponent)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util named-type)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 curried-definitions)
  :export (vline
           vline?
           vline-parameters vline-parameters*
           vline-value vline-value*
           vcomponent vcomponent?
           type type*
           vcomponent-children vcomponent-children*
           vcomponent-properties vcomponent-properties*

           vline-equal?
           vcomponent-equal?
           vcomponent-diff

           prop%
           prop*
           prop1
           extract1

           param*

           add-child
           ))

(define (serialize-vline v)
  `(vline value: ,(serialize (vline-value v))
          ,@(if (table-empty? (vline-parameters v))
                '()
                `(params: ,(serialize (vline-parameters v))))))


(define-type (vline serializer: serialize-vline)
  (vline-parameters default: (table)
                    type: table?
                    keyword: params)
  (vline-value keyword: value))

(define (vline-equal? a b)
  (and (equal? (vline-value a)
               (vline-value b))
       (table-equal? (vline-parameters a)
                     (vline-parameters b))))


(define-type (vcomponent ; serializer: serialize-vcomponent
              )
  (type type: symbol?)
  (vcomponent-properties default: (table (named-type (non-empty-list-of vline?)))
                         type: table?
                         ;; TODO remove this keyword, since it allows to create
                         ;; tables without the typecheck
                         keyword: properties)
  (vcomponent-children default: '() type: (list-of vcomponent?)
                       keyword: children))

(define (add-child parent child)
  (modify parent vcomponent-children*
          (lambda (c) (append c (list child)))))

(define (vcomponent-equal? a b)
  (equal? '() (vcomponent-diff a b)))

(define (sort-vcomponents-by-best-effort lst)
  (sort lst
        (lambda (a b)
          (cond ((eq? (type a) (type b))
                 (< (length (table->list (vcomponent-properties a)))
                    (length (table->list (vcomponent-properties b))))
                 ;; TODO further tests, maybe including
                 ;; - names of properties
                 ;; - values of properties (if all same name)
                 ;; - number of children
                 ;; - this function recursed on the children
                 )
                (else
                 (string< (symbol->string (type a))
                          (symbol->string (type b))))))))

(define (vcomponent-diff a b)
  (append
   (if (eqv? (type a) (type b))
       '()
       `((diff type ,(type a) ,(type b))))
   (table-diff
    (vcomponent-properties a)
    (vcomponent-properties b)
    (lambda (ax bx) (lset= vline-equal? ax bx)))
   (append-map vcomponent-diff
               (sort-vcomponents-by-best-effort (vcomponent-children a))
               (sort-vcomponents-by-best-effort (vcomponent-children b)))))

;;; Lenses
;;; - focus non-existant member of collection?
;;; - remove member through a lens

;;; Focus a component.
;;; Focused record is wrapped in a on `optional?`, to allow removal of properties
;;; If `just` an object is returned, it MUST be a non-empty list of vlines.
(define (prop* key)
  (lens-compose vcomponent-properties* (table-focus key)))

;;; Retrive all vlines for a key, or #f if not present.
;;; TODO rename this to simply `prop`, once all instances of the old `prop` is gone
(define (prop% component key)
  (unjust (get component (prop* key)) #f))

(define (prop1 component key)
  (unjust (get/preview component (prop* key) just* car* vline-value*)
          #f))

(define (extract1 key)
  (lambda (e) (prop1 e key)))

(define (param* key)
  (lens-compose vline-parameters* (table-focus key)))

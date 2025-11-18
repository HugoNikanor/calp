(define-module (vcomponent)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
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

           prop*
           prop1
           extract1

           param*

           add-child

           set-prop
                 )
   )

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
  ;; (table-of (non-empty-list-of vline?))
  (vcomponent-properties default: (table
                                   (lambda (l)
                                     (and (list? l)
                                          (every vline? l))))
                         type: table?
                         keyword: properties)
  (vcomponent-children default: '() type: (list-of vcomponent?)
                       keyword: children))

(define (add-child parent child)
  (modify parent vcomponent-children*
          (lambda (c) (append c (list child)))))

(define (vcomponent-equal? a b)
  (and (eqv? (type a) (type b))
       (table-equal?
        (vcomponent-properties a)
        (vcomponent-properties b)
        (lambda (ax bx) (lset= vline-equal? ax bx)))
       (lset= vcomponent-equal?
              (vcomponent-children a)
              (vcomponent-children b))))

(define (vcomponent-diff a b)
  (append
   (if (eqv? (type a) (type b))
       '()
       `(diff type ,(type a) ,(type b)))
   (table-diff
    (vcomponent-properties a)
    (vcomponent-properties b)
    (lambda (ax bx) (lset= vline-equal? ax bx)))
   ;; NOTE this assumes same order for children.
   ;; This isn't correct, but there is no obvious way to sort children
   (append-map vcomponent-diff
               (vcomponent-children a)
               (vcomponent-children b))))

;;; Lenses
;;; - focus non-existant member of collection?
;;; - remove member through a lens

;;; Focus a component.
;;; Focused record is wrapped in a on `optional?`, to allow removal of properties
;;; If `just` an object is returned, it MUST be a non-empty list of vlines.
(define (prop* key)
  (lens-compose vcomponent-properties* (table-focus key)))

(define (prop1 component key)
  (unjust (get/preview component (prop* key) just* car* vline-value*)
          #f))

(define (extract1 key)
  (lambda (e) (prop1 e key)))

;;; TODO actually write the prop1* lens
;;; The "obvious" implementation of
;;;     (define (prop1* key) (lens-compose (prop* key) just* car*))
;;; works as expected for `get`(`/preview`), but doesn't work for the
;;; modify case, since the `just*` can't focus on nothing.

;;; When accessed, focuses (optional-of vline?)
;;; If `just?` is returned, replace the car of the list with the value
;;; (or create it if it didn't exist before)
;;; If `nothing?` is returned,
;; (define (((prop1* key) component) op)
;;   (modify component (prop* key)
;;           (lambda (focus)
;;             (if (nothing? focus)
;;                 (cond ((op #f) => (compose just list))
;;                       (else (nothing)))
;;                 (cond ((op (car (from-just focus)))
;;                        => (lambda (n) (set focus (lens-compose just* car*) n)))
;;                       (else
;;                        (let ((old (from-just focus)))
;;                          (if (null? (cdr old))
;;                              (nothing)
;;                              ;; Is this sensible behaviour?
;;                              ;; Removing the first element
;;                              (modify focus just* cdr)))))))))


;; (define (set-prop1 component key value)
;;   (typecheck value vline?)
;;   (modify component (prop* key)
;;           (lambda (field)
;;             (cond ((just? field)
;;                    (set field (lens-compose just* car*)
;;                         value))
;;                   (else (just (list value)))))))

(define (set-prop component key values)
  (typecheck values (list-of vline?))
  (set component (prop* key)
       (just values)))


;; (define (children vcomponent)
;;   (vcomponent-children vcomponent))


;; (define (add-child parent child)
;;   (modify parent vcomponent-children*
;;           (lambda (ch) (cons (modify child parent* parent) ch))))

;;; remove-property

(define (param* key)
  (lens-compose vline-parameters* (table-focus key)))

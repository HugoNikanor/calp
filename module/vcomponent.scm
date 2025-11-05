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

           vcomponent-equal?

           prop*
           prop1
           extract1

           param*

           add-child

           set-prop
                 )
   )

;; (define (serialize-vline v)
;;   `(vline vline-value: ,(serialize (vline-value v))
;;           ,@(if (table-empty? (vline-parameters v))
;;                 '()
;;                 `(vline-parameters: ,(serialize (vline-parameters v))))))


(define-type (vline ; serializer: serialize-vline
              )
  (vline-parameters default: (table)
                    type: table?
                    keyword: params)
  (vline-value keyword: value)
  ; (vline-source default: "" type: string?)
  )

;; (define (vline-equal? a b)
;;   (and (equal? (vline-value a)
;;                (vline-value b))
;;        (equal? (sort* (table->list (vline-parameters a)) string<? (compose symbol->string car))
;;                (sort* (table->list (vline-parameters b)) string<? (compose symbol->string car)))))


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
                       keyword: children)
  ; (parent     default: #f      type: (or false? vcomponent?))
  )

(define (add-child parent child)
  (modify parent vcomponent-children*
          (lambda (c) (append c (list child)))))

(define (vcomponent-equal? a b)
  ;; TODO implement
  (throw 'not-implemented))

;; (define (vcomponent-equal? a b)
;;   (and (eqv? (type a) (type b))
;;        (= (length (vcomponent-children a)) (length (vcomponent-children b)))
;;        ;; TODO this doesn't work, since UID isn't guarnteed to exist
;;        (every vcomponent-equal?
;;             (sort* (vcomponent-children a) string< (extract 'UID))
;;             (sort* (vcomponent-children b) string< (extract 'UID)))
;;        (every (lambda (a b)
;;                 (and (eq? (car a) (car b))
;;                      (cond ((and (list? (cadr a))
;;                                  (list? (cadr b)))
;;                             (every vline-equal?
;;                                    (cadr a)
;;                                    (cadr b)))
;;                            ((and (not (list? (cadr a)))
;;                                  (not (list? (cadr b))))
;;                             (vline-equal? (cadr a)
;;                                           (cadr b)))
;;                            (else #f))))
;;               (table->list (vcomponent-properties (properties a)))
;;               (table->list (vcomponent-properties (properties b))))))


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

;;; Changes:
;;; - prop* is now a lens instead of an accessor
;;; - prop is removed
;;; - prop% is removed
;;; - children is a list again





;;; Alternative 1:
;;;   We embed each individual event in a calendar component
;;; Pros:
;;; - closer to how iCalendar wants to work
;;; - parity with vdir
;;; Cons:
;;; - "needless" calendar wrapper for each event
;;; - might seem weird for stores with multiple events in one store
;;;   (duplicate calendar object for each event?)

;;; Alternative 2:
;;;   We keep extract the vevents
;;; Pros:
;;; - less needless wrapper
;;; Cons:
;;; - x-hnh-alternatives
;;; - we lose data only present in the calendar part


;;; Consider: we have a list of data stores
;;; From each data stores, we load a number of VCALENDAR objects.

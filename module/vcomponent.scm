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

           vcalendar? vevent? vtodo? vjournal? vfreebusy?
           vtimezone? valarm? standard? daylight?
           ))

(define (serialize-vline v)
  `(vline value: ,(serialize (vline-value v))
          ,@(if (table-empty? (vline-parameters v))
                '()
                `(params: ,(serialize (vline-parameters v))))))


(define-type (vline serializer: serialize-vline)
  ;; NOTE adding the type clause to the table causes vline-equal?
  ;; to fail for seemingly identical lines.
  (vline-parameters default: (table #; (named-type string?))
                    type: table?
                    keyword: params)
  (vline-value keyword: value
               type: (not vline?)))

(define (vline-equal? a b)
  (and (equal? (vline-value a)
               (vline-value b))
       (table-equal? (vline-parameters a)
                     (vline-parameters b))))


(define (serialize-vcomponent c)

  (define (serialize-vline* vline)
    (if (table-empty? (vline-parameters vline))
        (serialize (vline-value vline))
        `(with-parameters
          ,@(concatenate
             (map (lambda (pair)
                    (list (symbol->keyword (downcase-symbol (car pair)))
                          (serialize (cdr pair))))
                  (table->list (vline-parameters vline))))
          ,(serialize (vline-value vline)))))

  `(,@(if (memv (downcase-symbol (type c))
             '(vcalendar vevent vtodo vjournal vfreebusy vtimezone valarm standard daylight))
          `(,(downcase-symbol (type c)))
          `(create-vcomponent ',(type c)))

    ,@(concatenate
       (map (lambda (p)
              (define-values (key lines) (car+cdr p))
              `(,(symbol->keyword (downcase-symbol key))
                ,(if (null? (cdr lines))
                     (serialize-vline* (car lines))
                     `(list ,@(map serialize-vline* lines)))))
            (table->list (vcomponent-properties c))))

    ,@(if (null? (vcomponent-children c))
          '()
          `((list ,@(map serialize (vcomponent-children c)))))))

(define-type (vcomponent serializer: serialize-vcomponent)
  (type type: symbol?)
  (vcomponent-properties default: (table (named-type (non-empty-list-of vline?)))
                         type: (table-of (non-empty-list-of vline?))
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

(define (vcalendar? x) (and (vcomponent? x) (eq? 'VCALENDAR (type x))))
(define (vevent?    x) (and (vcomponent? x) (eq? 'VEVENT    (type x))))
(define (vtodo?     x) (and (vcomponent? x) (eq? 'VTODO     (type x))))
(define (vjournal?  x) (and (vcomponent? x) (eq? 'VJOURNAL  (type x))))
(define (vfreebusy? x) (and (vcomponent? x) (eq? 'VFREEBUSY (type x))))
(define (vtimezone? x) (and (vcomponent? x) (eq? 'VTIMEZONE (type x))))
(define (valarm?    x) (and (vcomponent? x) (eq? 'VALARM    (type x))))
(define (standard?  x) (and (vcomponent? x) (eq? 'STANDARD  (type x))))
(define (daylight?  x) (and (vcomponent? x) (eq? 'DAYLIGHT  (type x))))

(define-module (vcomponent)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util named-type)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util serialize)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 curried-definitions)
  :use-module (datetime)
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
           param

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
             (table->list
              (vline-parameters vline)
              (lambda (key value)
                (list (symbol->keyword (downcase-symbol key))
                      (serialize value)))))
          ,(serialize (vline-value vline)))))

  `(,@(if (memv (downcase-symbol (type c))
             '(vcalendar vevent vtodo vjournal vfreebusy vtimezone valarm standard daylight))
          `(,(downcase-symbol (type c)))
          `(create-vcomponent ',(type c)))

    ,@(concatenate
       (table->list
        (vcomponent-properties c)
        (lambda (key lines)
          `(,(symbol->keyword (downcase-symbol key))
            ,(if (null? (cdr lines))
                 (serialize-vline* (car lines))
                 `(list ,@(map serialize-vline* lines)))))))

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

;;; NOTE this is borrowed from (vcomponent datetime).
;;; It's needed here for sort-vcomponent-by-best-effort, but can't be
;;; imported since that would result in a dependency loop.
;;; The final solution should probably be to merge vcomponent-datetime
;;; into this module.
(define (instance-start-datetime reference-zone instance)
  (typecheck reference-zone string?)
  (typecheck instance vevent?)

  (let ((s (prop1 instance 'DTSTART)))
    (cond ((date? s) (datetime date: s tz: reference-zone))
          ((unzoned-datetime? s) (tz s reference-zone))
          (else                         ; guaranteed zoned datetime
           s))))


(define (sort-vcomponents-by-best-effort lst)
  (cond ((null? lst) '())
        ((not (apply eq? (map type lst)))
         (scm-error 'misc-error "sort-vcomponents-by-best-effort"
                    "Can only sort vcomponents of identical type, got ~s"
                    (list (map type lst)) #f))
        (else (case (type (car lst))
                ((VEVENT) (sort* lst datetime<
                                 (lambda (x) (zone->utc
                                         ;; Hard coding UTC here is valid,
                                         ;; since we only care about about a
                                         ;; ordering which doesn't change, which
                                         ;; any (non-changing) timezone provides.
                                         (instance-start-datetime "UTC" x)))))
                ;; TODO there are actually ways to sort other components
                (else lst)))
        ))

(define* (vcomponent-diff a b key: table-report)
  (append
   (if (eqv? (type a) (type b))
       '()
       `((*type* ,(type a) ,(type b))))
   (append
    (map (lambda (diff) (cons '*properties* diff))
         (apply table-diff
                (vcomponent-properties a)
                (vcomponent-properties b)
                compare: (lambda (ax bx) (lset= vline-equal? ax bx))
                (if table-report
                    `(report: ,table-report)
                    '())))

    (let ((l c r
             (table-venn-partition
              (group-by/table type (vcomponent-children a))
              (group-by/table type (vcomponent-children b)))))
      (append
       (table->list l (lambda (_ v) (list (type (car v)) '*left-only*  (length v))))
       (table->list r (lambda (_ v) (list (type (car v)) '*right-only* (length v))))
       (concatenate
        (table->list
         c (lambda (type p)
             (cond ((= (length (car p)) (length (cdr p)))
                    (append-map
                     (lambda (a b i)
                       (map (lambda (diff) (cons `(,type ,i) diff))
                            (vcomponent-diff a b)))
                     (sort-vcomponents-by-best-effort (car p))
                     (sort-vcomponents-by-best-effort (cdr p))
                     (iota (length (car p)))))
                   (else
                    `(,type
                      *child-list-length-diff*
                      ,(length (car p))
                      ,(length (cdr p)))))))))))))

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

(define param
  (case-lambda
    ((vline key)
     (unjust (get vline (param* key)) #f))
    ((vline key value)
     (set vline (param* key) (just value)))))

(define (vcalendar? x) (and (vcomponent? x) (eq? 'VCALENDAR (type x))))
(define (vevent?    x) (and (vcomponent? x) (eq? 'VEVENT    (type x))))
(define (vtodo?     x) (and (vcomponent? x) (eq? 'VTODO     (type x))))
(define (vjournal?  x) (and (vcomponent? x) (eq? 'VJOURNAL  (type x))))
(define (vfreebusy? x) (and (vcomponent? x) (eq? 'VFREEBUSY (type x))))
(define (vtimezone? x) (and (vcomponent? x) (eq? 'VTIMEZONE (type x))))
(define (valarm?    x) (and (vcomponent? x) (eq? 'VALARM    (type x))))
(define (standard?  x) (and (vcomponent? x) (eq? 'STANDARD  (type x))))
(define (daylight?  x) (and (vcomponent? x) (eq? 'DAYLIGHT  (type x))))

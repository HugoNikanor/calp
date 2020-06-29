(define-module (vcomponent base)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (srfi srfi-17)
  :use-module (ice-9 hash-table)
  :use-module ((ice-9 optargs) :select (define*-public))
  )



;;; <vcomponent>
;;;   <properties>
;;;     <dtstart>
;;;       <parameters>
;;;         <tzid><text>Europe/Stockholm</text></tzid>
;;;       </parameters>
;;;       2020-01-01T13:37:50
;;;     </dtstart>
;;;   </properties>
;;; </vcomponent>
;;;

;; The <vline> type is a bit to many times refered to as a attr ptr.
(define-record-type <vline>
  (make-vline% key value parameters)
  vline?
  (key vline-key)
  (value get-vline-value set-vline-value!)
  (parameters get-vline-parameters)
  (source get-source set-source!)
  )

(export vline-key)

(set-record-type-printer!
 <vline>
 (lambda (v p)
   (format p "#<<vline> key: ~s value: ~s parameters: ~s>"
           (vline-key v)
           (get-vline-value v)
           (hash-map->list list (get-vline-parameters v)))))

(define-public vline-source
  (make-procedure-with-setter
   get-source set-source!))

(define*-public (make-vline key value #:optional (ht (make-hash-table)))
  (make-vline% key value ht))

(define-record-type <vcomponent>
  (make-vcomponent% type children parent properties)
  vcomponent?
  (type type)
  (children children set-component-children!)
  (parent get-component-parent set-component-parent!)
  (properties get-component-properties))
(export vcomponent? children type)

((@ (srfi srfi-9 gnu) set-record-type-printer!)
 <vcomponent>
 (lambda (c p)
   (format p "#<<vcomponent> ~a, len(child)=~a, parent=~a>~%"
           (type c)
           (length (children c))
           (and=> (get-component-parent c) type))))

;; TODO should this also update the parent
(define-public parent
  (make-procedure-with-setter
   get-component-parent set-component-parent!))

(define*-public (make-vcomponent #:optional (type 'VIRTUAL))
  (make-vcomponent% type '() #f (make-hash-table)))

(define-public (add-child! parent child)
  (set-component-children! parent (cons child (children parent)))
  (set-component-parent! child parent))

;; TODO this doesn't handle multi-valued items
(define* (get-property-value component key #:optional default)
  (cond [(hashq-ref (get-component-properties component)
                    key #f)
         => get-vline-value]
        [else default]))

(define (get-property component key)
  (hashq-ref (get-component-properties component)
             key))

(define (set-property! component key value)
  (let ((ht (get-component-properties component)))
   (cond [(hashq-ref ht key #f)
          => (lambda (vline) (set-vline-value! vline value))]
         [else (hashq-set! ht key (make-vline key value))])))

(define-public (set-vline! component key vline)
  (hashq-set! (get-component-properties component)
              key vline))



;; vline → value
(define-public value
  (make-procedure-with-setter
   get-vline-value set-vline-value!))

;;; TODO all these set-attr should be set-prop, but
;;; set-prop is already used by what should be set-param.

;; vcomponent x (or str symb) → vline
(define (get-attr* component attr)
  (hashq-ref (get-component-properties component)
             (as-symb attr)))

(define (set-attr*! component key value)
  (hashq-set! (get-component-properties component)
              (as-symb key) value))

(define-public attr*
  (make-procedure-with-setter
   get-attr*
   set-attr*!))

;; vcomponent x (or str symb) → value
(define (get-attr component key)
  (let ((attrs (get-attr* component key)))
    (cond [(not attrs) #f]
          [(list? attrs) (map value attrs)]
          [else (value attrs)])))

;; TODO do something sensible here
(define (set-attr! component key value)
  (set-property! component (as-symb key) value))

(define-public attr
  (make-procedure-with-setter
   get-attr
   set-attr!))


(define-public param
  (make-procedure-with-setter
   (lambda (vline parameter-key)
     ;; TODO `list' is a hack since a bit to much code depends
     ;; on prop always returning a list of values.
     (and=> (hashq-ref (get-vline-parameters vline)
                       (as-symb parameter-key))
            list))
   (lambda (vline parameter-key val)
     (hashq-set! (get-vline-parameters vline)
                 (as-symb parameter-key) val))))

;; Returns the properties of attribute as an assoc list.
;; @code{(map car <>)} leads to available properties.
;; TODO shouldn't this be called parameters?
(define-public (parameters attrptr)
  (hash-map->list list (get-vline-parameters attrptr)))

(define-public (properties component)
  (get-component-properties component))

(define-public (property-keys component)
  (map car (hash-map->list cons (get-component-properties component))))

(define (copy-vline vline)
  (make-vline (vline-key vline)
              (get-vline-value vline)
              ;; TODO deep-copy on parameters?
              (get-vline-parameters vline)))

(define-public (copy-vcomponent component)
  (make-vcomponent%
   (type component)
   (children component)
   (parent component)
   ;; properties
   (alist->hashq-table
    (hash-map->list (lambda (key value)
                      (cons key (if (list? value)
                                    (map copy-vline value)
                                    (copy-vline value))))
                    (get-component-properties component)))))

(define-public (extract field)
  (lambda (e) (attr e field)))

(define-public (extract* field)
  (lambda (e) (attr* e field)))

(define-public (key=? k1 k2)
  (eq? (as-symb k1)
       (as-symb k2)))

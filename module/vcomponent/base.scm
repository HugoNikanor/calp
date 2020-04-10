(define-module (vcomponent base)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-17)
  :use-module (ice-9 hash-table)
  :use-module ((ice-9 optargs) :select (define*-public))
  )



;; The <vline> type is a bit to many times refered to as a attr ptr.
(define-record-type <vline>
  (make-vline% value parameters)
  vline?
  (value get-vline-value set-vline-value!)
  (parameters get-vline-parameters)
  ;; TODO Add slot for optional source object, containing
  ;; - file of origin
  ;; - position in that file
  ;; - source string, before value parsing.
  )

(define*-public (make-vline value #:optional (ht (make-hash-table)))
  (make-vline% value ht))

(define-record-type <vcomponent>
  (make-vcomponent% type children parent attributes)
  vcomponent?
  (type type)
  (children children set-component-children!)
  (parent get-component-parent set-component-parent!)
  (attributes get-component-attributes))
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

(define* (get-attribute-value component key #:optional default)
  (cond [(hashq-ref (get-component-attributes component)
                    key #f)
         => get-vline-value]
        [else default]))

(define (get-attribute component key)
  (hashq-ref (get-component-attributes component)
             key))

(define (set-attribute! component key value)
  (let ((ht (get-component-attributes component)))
   (cond [(hashq-ref ht key #f)
          => (lambda (vline) (set-vline-value! vline value))]
         [else (hashq-set! ht key (make-vline value))])))

(define-public (set-vline! component key vline)
  (hashq-set! (get-component-attributes component)
              key vline))



;; vline → value
(define-public value
  (make-procedure-with-setter
   get-vline-value set-vline-value!))

;; vcomponent x (or str symb) → vline
(define-public (attr* component attr)
  (hashq-ref (get-component-attributes component)
             (as-symb attr)))

;; vcomponent x (or str symb) → value
(define (get-attr component key)
  (get-attribute-value component (as-symb key) #f))

(define (set-attr! component key value)
  (set-attribute! component (as-symb key) value))

(define-public attr
  (make-procedure-with-setter
   get-attr
   set-attr!))


(define-public prop
  (make-procedure-with-setter
   (lambda (attr-obj prop-key)
     ;; TODO `list' is a hack since a bit to much code depends
     ;; on prop always returning a list of values.
     (and=> (hashq-ref (get-vline-parameters attr-obj)
                       (as-symb prop-key))
            list))
   (lambda (attr-obj prop-key val)
     (hashq-set! (get-vline-parameters attr-obj)
                 (as-symb prop-key) val))))

;; Returns the properties of attribute as an assoc list.
;; @code{(map car <>)} leads to available properties.
(define-public (properties attrptr)
  (hash-map->list list (get-vline-parameters attrptr)))

(define-public (attributes component)
  (get-component-attributes component))

(define-public (attribute-keys component)
  (map car (hash-map->list cons (get-component-attributes component))))

(define (copy-vline vline)
  (make-vline (get-vline-value vline)
              ;; TODO deep-copy on properties?
              (get-vline-parameters vline)))

(define-public (copy-vcomponent component)
  (make-vcomponent% (type component)
                    (children component)
                    (parent component)
                    ;; attributes
                    (alist->hashq-table
                     (hash-map->list (lambda (key value) (cons key (copy-vline value)))
                                     (get-component-attributes component)))))

(define-public (extract field)
  (lambda (e) (attr e field)))

(define-public (extract* field)
  (lambda (e) (attr* e field)))

(define-public (key=? k1 k2)
  (eq? (as-symb k1)
       (as-symb k2)))

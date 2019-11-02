(define-module (vcomponent base)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module ((vcomponent parse)
               :renamer (lambda (symb)
                          (case symb
                            ;; [(set-attribute!) 'get-attribute]
                            [(make-vcomponent) 'primitive-make-vcomponent]
                            [else symb])))
  :use-module (ice-9 hash-table)
  :use-module ((ice-9 optargs) :select (define*-public))
  :re-export (add-child! primitive-make-vcomponent))

(define-public (parse-cal-path path)
  (let ((parent (primitive-make-vcomponent)))
    (for-each (lambda (child) (add-child! parent child))
              (read-vcalendar path))
    (if (null? (get-component-children parent))
        (set-attribute! parent 'X-HNH-SOURCETYPE "vdir")
        (set-attribute! parent 'X-HNH-SOURCETYPE
                        (get-attribute-value (car (get-component-children parent))
                                             'X-HNH-SOURCETYPE "vdir")))
    parent))

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
  (hash-map->list cons (get-attribute-parameters attrptr)))

(define-public type (make-procedure-with-setter
                     (lambda (c) (component-type c))
                     (lambda (c v) ; struct-set! c 0 v
                       (format (current-error-port)
                               "This method is a deprecated NOOP"))))

(define-public parent get-component-parent)

(define-public (attributes component)
  (hash-map->list cons (get-component-attributes component)))

(define*-public children get-component-children)

(define (copy-vline vline)
  (make-vline (get-vline-value vline)
              ;; TODO deep-copy on properties?
              (get-vline-parameters vline)))

(define-public (copy-vcomponent component)
  (make-vcomponent% (component-type component)
                    (get-component-children component)
                    (get-component-parent component)
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

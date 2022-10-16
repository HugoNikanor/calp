(define-module (vcomponent base)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-88)
  :use-module (ice-9 hash-table)
  :export (make-vline
           vline?
           vline-key
           vline-source

           make-vcomponent
           vcomponent?
           children type parent

           add-child! remove-child!

           delete-property!
           prop* prop
           extract extract*

           delete-parameter!
           value
           param

           parameters
           properties

           copy-vcomponent
           x-property?
           internal-field?


           )
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

(define-record-type <vline>
  (make-vline% key value parameters)
  vline?
  (key vline-key)
  (value get-vline-value set-vline-value!)
  (parameters get-vline-parameters)
  (source get-source set-source!)
  )

(set-record-type-printer!
 <vline>
 (lambda (v p)
   (format p "#<<vline> key: ~s value: ~s parameters: ~s>"
           (vline-key v)
           (get-vline-value v)
           (hash-map->list list (get-vline-parameters v)))))

(define vline-source
  (make-procedure-with-setter
   get-source set-source!))

(define* (make-vline key value optional: (ht (make-hash-table)))
  (make-vline% key value ht))

(define-record-type <vcomponent>
  (make-vcomponent% type children parent properties)
  vcomponent?
  (type type)
  (children children set-component-children!)
  (parent get-component-parent set-component-parent!)
  (properties get-component-properties))

((@ (srfi srfi-9 gnu) set-record-type-printer!)
 <vcomponent>
 (lambda (c p)
   (format p "#<<vcomponent> ~a, len(child)=~a, parent=~a>"
           (type c)
           (length (children c))
           (and=> (get-component-parent c) type))))

;; TODO should this also update the parent
(define parent
  (make-procedure-with-setter
   get-component-parent set-component-parent!))

(define* (make-vcomponent optional: (type 'VIRTUAL))
  (make-vcomponent% type '() #f (make-hash-table)))

(define (add-child! parent child)
  (set-component-children! parent (cons child (children parent)))
  (set-component-parent! child parent))

(define (remove-child! parent-component child)
  (unless (eq? parent-component (parent child))
    (scm-error
     'wrong-type-arg "remove-child!" "Child doesn't belong to parent"
     (list parent-component child) #f))
  (set-component-children! parent-component (delq1! child (children parent-component)))
  (set-component-parent! child #f))

;;; TODO key=DTSTART, (date? value) => #t
;;; KRÄVER att (props vline 'VALUE) <- "DATE"
(define (set-property! component key value)
  (let ((ht (get-component-properties component)))
   (cond [(hashq-ref ht key #f)
          => (lambda (vline) (set-vline-value! vline value))]
         [else (hashq-set! ht key (make-vline key value))])))




;; vline → value
(define value
  (make-procedure-with-setter
   get-vline-value set-vline-value!))

;; vcomponent x (or str symb) → vline
(define (get-prop* component prop)
  (hashq-ref (get-component-properties component)
             (as-symb prop)))

(define (set-prop*! component key value)
  (hashq-set! (get-component-properties component)
              (as-symb key) value))

(define prop*
  (make-procedure-with-setter
   get-prop*
   set-prop*!))

(define (delete-property! component key)
  (hashq-remove! (get-component-properties component)
                 (as-symb key)))


;; vcomponent x (or str symb) → value
(define (get-prop component key)
  (let ((props (get-prop* component key)))
    (cond [(not props) #f]
          [(list? props) (map value props)]
          [else (value props)])))

;; TODO do something sensible here
(define (set-prop! component key value)
  (set-property! component (as-symb key) value))

(define prop
  (make-procedure-with-setter
   get-prop
   set-prop!))


(define param
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


(define (delete-parameter! vline parameter-key)
  (hashq-remove! (get-vline-parameters vline)
                 (as-symb parameter-key)))


;; Returns the parameters of a property as an assoc list.
;; @code{(map car <>)} leads to available parameters.
(define (parameters vline)
  (hash-map->list list (get-vline-parameters vline)))

(define (properties component)
  (hash-map->list cons (get-component-properties component)))

(define (copy-vline vline)
  (make-vline (vline-key vline)
              (get-vline-value vline)
              ;; TODO deep-copy on parameters?
              (get-vline-parameters vline)))

(define (copy-vcomponent component)
  (make-vcomponent%
   (type component)
   ;; TODO deep copy?
   (children component)
   (parent component)
   ;; properties
   (alist->hashq-table
    (hash-map->list (lambda (key value)
                      (cons key (if (list? value)
                                    (map copy-vline value)
                                    (copy-vline value))))
                    (get-component-properties component)))))

(define (extract field)
  (lambda (e) (prop e field)))

(define (extract* field)
  (lambda (e) (prop* e field)))

(define (x-property? symb)
  (string=? "X-" (string-take (symbol->string symb) 2)))

(define* (internal-field? symbol optional: (prefix "-"))
  (string=? prefix
            (string-take-to (symbol->string symbol)
                            (string-length prefix))))

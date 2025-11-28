(define-module (vcomponent create)
  :use-module ((vcomponent) :prefix vcs-)
  :use-module ((vcomponent)
               :select (vline vline?
                              add-child
                              prop*
                              ))
  :use-module ((srfi srfi-1) :select (fold last drop-right car+cdr every concatenate))
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util table) :select (table alist->table table?))
  :use-module ((hnh util) :select (swap init+last kvlist->assq ->
                                        upcase-symbol))
  :use-module (hnh util type)
  :use-module (hnh util optional)
  :use-module (hnh util lens)
  :export (with-parameters
           create-vcomponent
           vcalendar vevent
           vtodo vjournal vfreebusy
           vtimezone valarm standard daylight
           ))



;; Upcase the keys in an association list. Keys must be symbols.
(define (upcase-keys alist)
  (map (cut modify <> car* upcase-symbol)
       alist))

;;; Macro to access pattern matching, could easily be a procedure.
(define-syntax-rule (with-parameters kvs ... value)
  (vline params: (-> (list kvs ...) kvlist->assq upcase-keys alist->table)
         value: value) )

(define (create-vcomponent type . attrs*)
  ;; Split the subforms into attributes and children
  (define-values (attrs children)
    (cond ((null? attrs*)          (values '() '()))
          ((even? (length attrs*)) (values attrs* '()))
          (else                    (init+last attrs*))))

  (define (value->vline value)
    (cond
     ((list? value)
      (concatenate (map value->vline value)))
     ((vline? value)
      (list value))
     (else
      (list
       (vline value: value)))))

  ;; For a given (symbol, value) pair, attach it to the given component
  (define (attach-property pair component)
    (let ((k value (car+cdr pair)))
      (modify component (prop* k)
              (lambda (f)
                (just (append (unjust f '())
                              (value->vline value)))))))

  (fold attach-property
        (vcs-vcomponent type: type children: children)
        (upcase-keys (kvlist->assq attrs))))

(define (vcalendar . attrs)
  (apply create-vcomponent 'VCALENDAR attrs))
(define (vevent . attrs)
  (apply create-vcomponent 'VEVENT attrs))
(define (vtodo . attrs)
  (apply create-vcomponent 'VTODO attrs))
(define (vjournal . attrs)
  (apply create-vcomponent 'VJOURNAL attrs))
(define (vfreebusy . attrs)
  (apply create-vcomponent 'VFREEBUSY attrs))
(define (vtimezone . attrs)
  (apply create-vcomponent 'VTIMEZONE attrs))
(define (valarm . attrs)
  (apply create-vcomponent 'VALARM attrs))
(define (standard . attrs)
  (apply create-vcomponent 'STANDARD attrs))
(define (daylight . attrs)
  (apply create-vcomponent 'DAYLIGHT attrs))

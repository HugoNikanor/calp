(define-module (vcomponent create)
  :use-module ((vcomponent base) :prefix vcs-)
  :use-module ((vcomponent base)
               :select (vline key add-child prop* vline?))
  :use-module ((srfi srfi-1) :select (fold last drop-right car+cdr))
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util table) :select (alist->table table?))
  :use-module ((hnh util) :select (swap init+last kvlist->assq ->))
  :use-module (hnh util object)
  :use-module (hnh util type)
  :export (with-parameters
           as-list
           create-vcomponent
           vcalendar vevent
           vtimezone standard daylight
           ))



;; Convert a scheme keyword to a symbol suitable for us
(define (keyword->key keyword)
  (-> keyword
      keyword->string
      string-upcase                     ; NOCOV
      string->symbol))

(define (symbol-upcase symbol)
  (-> symbol
      symbol->string
      string-upcase                     ; NOCOV
      string->symbol))

;; Upcase the keys in an association list. Keys must be symbols.
(define (upcase-keys alist)
  (map (lambda (pair) (cons (symbol-upcase (car pair))
                       (cdr pair)))
       alist))



(define (kvlist->parameter-table kvs)
  (-> kvs kvlist->assq upcase-keys alist->table))

(define-type (parameterized)
  parameterized:value
  (parameterized:parameters type: table?))

;;; This is implemented as a macro, with an external typecheck, due to
;;; how *when* Guile interprets different things. The check for list-value?
;;; fails since Guile thinks it's a syntax deffinition at this point.
;;; This setup waits with actually looking up list-value?, meaning that the
;;; symbol is a procedure when the code is actually ran.
(define-syntax with-parameters
  (syntax-rules ()
    ((_ kvs ... value)
     (begin
       (typecheck value (not (or list-value?
                                 parameterized?
                                 vline?)))
       (parameterized
        parameterized:value: value
        parameterized:parameters: (kvlist->parameter-table (list kvs ...)))))))




(define-type (list-value)
  (list-value-value type: (list-of (not list-value?))))

(define (as-list arg)
  (list-value list-value-value: arg))



(define (create-vcomponent type . attrs*)
  ;; Split the subforms into attributes and children
  (define-values (attrs children)
    (cond ((null? attrs*)          (values '() '()))
          ((even? (length attrs*)) (values attrs* '()))
          (else                    (init+last attrs*))))

  (define (value->vline key value)
    (cond
     ((vline? value)
      (scm-error 'misc-error "create-vcomponent"
                 "Explicit VLines should never appear when creating components: ~s"
                 (list value) #f))

     ((list-value? value)
      (scm-error 'misc-error "create-vcomponent"
                 "As-list can only be used at top level. key: ~s, value: ~s"
                 (list key value) #f))

     ((parameterized? value)
      (vline key: key
             vline-value: (parameterized:value value)
             vline-parameters: (parameterized:parameters value)))

     ;; A raw value was given, embed it into a vline
     (else (vline key: key vline-value: value))))

  ;; For a given (symbol, value) pair, attach it to the given component
  (define (attach-property pair component)
    (let ((k value (car+cdr pair)))
      (cond
       ((and (list-value? value) (null? (list-value-value value)))
        component)

       ((list-value? value)
        (prop* component k
               (map (lambda (v) (value->vline k v))
                    (list-value-value value))))

       (else
        (prop* component k (value->vline k value))))))

  ;; TODO add-child requires a UID on the child
  ;; Possibly just genenerate one here if missing
  (fold (swap add-child)
        (fold attach-property
              (vcs-vcomponent type: type)
              (upcase-keys (kvlist->assq attrs)))
        children))

(define (vcalendar . attrs)
  (apply create-vcomponent 'VCALENDAR attrs))

(define (vevent . attrs)
  (apply create-vcomponent 'VEVENT attrs))

(define (vtimezone . attrs)
  (apply create-vcomponent 'VTIMEZONE attrs))

(define (standard . attrs)
  (apply create-vcomponent 'STANDARD attrs))

(define (daylight . attrs)
  (apply create-vcomponent 'DAYLIGHT attrs))

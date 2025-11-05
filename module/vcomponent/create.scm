(define-module (vcomponent create)
  :use-module ((vcomponent) :prefix vcs-)
  :use-module ((vcomponent)
               :select (vline vline?
                              add-child
                              prop*
                              ))
  :use-module ((srfi srfi-1) :select (fold last drop-right car+cdr every))
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util table) :select (table alist->table table?))
  :use-module ((hnh util) :select (swap init+last kvlist->assq ->))
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (hnh util optional)
  :use-module (hnh util lens)
  :export (with-parameters
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
  (map (cut modify <> car* symbol-upcase)
       alist))



(define (kvlist->parameter-table kvs)
  (-> kvs kvlist->assq upcase-keys alist->table))

(define-type (parameterized)
  (parameterized:value keyword: value)
  (parameterized:parameters keyword: params type: table?))

;;; This is implemented as a macro, with an external typecheck, due to
;;; how *when* Guile interprets different things. The check for list-value?
;;; fails since Guile thinks it's a syntax deffinition at this point.
;;; This setup waits with actually looking up list-value?, meaning that the
;;; symbol is a procedure when the code is actually ran.

;;; TODO above comment mentions now removed typecheck
;;; TODO try removing this, and simply using vlines directly
(define-syntax with-parameters
  (syntax-rules ()
    ((_ kvs ... value)
     (parameterized
      value: value
      params: (kvlist->parameter-table (list kvs ...))))))




(define (create-vcomponent type . attrs*)
  ;; Split the subforms into attributes and children
  (define-values (attrs children)
    (cond ((null? attrs*)          (values '() '()))
          ((even? (length attrs*)) (values attrs* '()))
          (else                    (init+last attrs*))))

  (define (value->vline value)
    (cond
     ((list? value)
      (map value->vline value))
     ((parameterized? value)
      (list
       (vline value: (parameterized:value value)
              params: (parameterized:parameters value))))
     (else
      (list
       (vline value: value)))))

  ;; For a given (symbol, value) pair, attach it to the given component
  (define (attach-property pair component)
    (let ((k value (car+cdr pair)))
      (modify component (prop* k)
              (lambda (f)
                (just (append (unjust f '()) (value->vline value)))))))

  (fold (lambda (child parent) (add-child parent child))
        (fold attach-property
              (vcs-vcomponent type: type
                              properties:
                              (table (lambda (l)
                                       (and (list? l)
                                            (every vline? l)))))
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

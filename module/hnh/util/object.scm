(define-module (hnh util object)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9 gnu)
  :use-module (ice-9 curried-definitions)
  :use-module (hnh util)
  :export (define-type
            build-validator-body
            list-of pair-of))



;; If given a syntax list extract the first lexeme, if given a "symbol", return that.
(define (syntax-first stx)
  (syntax-case stx ()
    ((a rest ...) #'a)
    (a #'a)))

(define (construct-syntax stx base transform)
  (->> base
       syntax->datum
       (format #f transform)
       string->symbol
       (datum->syntax stx)))

;; stx should be a syntax object of a key-value list on the form
;; (key: value key2: value2)
;; and target-key the datum which the target key unwraps to.
;; returns the corresponding values syntax
;; or #f if none is found
(define (kv-ref stx target-key)
  (syntax-case stx ()
    ((key value rest ...)
     (if (eqv? target-key (syntax->datum #'key))
         #'value
         (kv-ref #'(rest ...) target-key)))
    (_ #f)))



(define-syntax list-of
  (syntax-rules ()
    ((_ variable (rule ...))
     (and (list? variable)
          (every (lambda (x) (build-validator-body x (rule ...))) variable)))
    ((_ variable rule)
     (and (list? variable)
          (every rule variable)))))

(define-syntax-rule (pair-of variable a b)
  (and (pair? variable)
       (build-validator-body (car variable) a)
       (build-validator-body (cdr variable) b)))

;; DSL for specifying type predicates
;; Basically a procedure body, but the variable to test is implicit.
(define-syntax build-validator-body
  (syntax-rules (and or list-of)
    ((_ variable (and clauses ...))  (and (build-validator-body variable clauses) ...))
    ((_ variable (or clauses ...))   (or (build-validator-body variable clauses) ...))
    ((_ variable (proc args ...))    (proc variable args ...))
    ((_ variable proc)               (proc variable))))


;; Given (x type: predicate?), expand to a single `unless' form (otherwise #f)
(define-syntax (validator stx)
  (syntax-case stx ()
    ((_ (name kvs ...))
     (cond ((kv-ref #'(kvs ...) type:)
            => (lambda (type-stx)
                 (with-syntax ((type type-stx))
                   #'(unless (build-validator-body name type)
                       (scm-error 'wrong-type-arg "validator"
                                  "Invalid value for `~s'. Expected ~s, got ~s"
                                  (list (quote name) (quote type) name) #f)))))
           (else #f)))
    ((_ name) #f)))




;; Get syntax for getter-procedure's symbol
(define-syntax (field-get stx)
  (syntax-case stx ()
    ;; ((_ (name kv ...)) #'(field-get name))
    ((_ type-name name)
     (->>
      (format #f "~a-~a-get"
              (syntax->datum #'type-name)
              (syntax->datum #'name))
      string->symbol
      (datum->syntax stx)))))

;; get syntax for setter-procedure's symbol
(define-syntax (field-set stx)
  (syntax-case stx ()
    ;; ((_ (name kv ...)) #'(field-set name))
    ((_ type-name name)
     (->>
      (format #f "~a-~a-set"
              (syntax->datum #'type-name)
              (syntax->datum #'name))
      string->symbol
      (datum->syntax stx)))))

;; Construct a field line for define-immutable-record-type
(define ((field-declaration type) stx)
  (syntax-case stx ()
    (name
     (with-syntax ((name-get (->> (format #f "~a-~a-get"
                                          (syntax->datum type)
                                          (syntax->datum #'name))
                                  string->symbol
                                  (datum->syntax stx)))
                   (name-set (->> (format #f "~a-~a-set"
                                          (syntax->datum type)
                                          (syntax->datum #'name))
                                  string->symbol
                                  (datum->syntax stx))))
       #'(name name-get name-set)))))

;; Accessors are procedures for getting and setting fields in records
(define-syntax (build-accessor stx)
  (syntax-case stx ()
    ((_ type-name (name kvs ...))
     #'(define name
         (case-lambda ((datum)
                       ((field-get type-name name) datum))
                      ((datum new-value)
                       ;; validator uses the first field (in the list) as both
                       ;; the name of the field, and a reference to the value of
                       ;; the field. This ensures those two are the same for validator,
                       ;; while keeping name bound to the accessor in the outer scope.
                       (let ((name new-value))
                         (validator (name kvs ...)))
                       ((field-set type-name name) datum new-value)))))
    ((_ type-name name) #'(build-accessor type-name (name)))))


;; Go from my concept of field deffinitions, to what lambda* wants as arguments
(define (lambda*-stx field)
  (syntax-case field ()
    ((name kvs ...)
     (cond ((kv-ref #'(kvs ...) default:)
            => (lambda (dflt) #`(name #,dflt)))
           (else #'name)))
    (name #'name)))



(define-syntax (define-type stx)
  (syntax-case stx ()
    ((_ (name attribute ...) field ...)
     ;; These names SHOULD leak
     (with-syntax ((<type>? (construct-syntax stx #'name "~a?")))
       ;; These names are manually constructed, since generated identifiers are
       ;; only dependant on the source from which they orginate, which leads to
       ;; multiple instances of <type> being equal for similar types...
       ;; See the manual 6.10.10 Hygiene and the Top-Level
       (with-syntax ((<type>      (construct-syntax stx #'name "<~a>"))
                     (make-<type> (construct-syntax stx #'name "make-~a%")))
         #`(begin
             (define-immutable-record-type <type>
               (make-<type> #,@(map syntax-first #'(field ...)))
               <type>?
               #,@(map (field-declaration #'name)
                       (map syntax-first #'(field ...))))

             ;; User-facing constructor
             (define name
               #,(cond ((kv-ref #'(attribute ...) constructor:)
                        => (lambda (constructor-builder)
                             #`(#,constructor-builder
                                ;; primitive constructor
                                make-<type>
                                ;; Type validator
                                (lambda #,(map syntax-first #'(field ...))
                                  (validator field) ...))))
                       (else #`(lambda* (key: #,@(map lambda*-stx #'(field ...)))
                                 ;; Type validators
                                 (validator field) ...
                                 (make-<type> #,@(map syntax-first #'(field ...)))))))

             ;; Field accessors
             (build-accessor name field) ...

             ;; if printer in attribute
             #,@(cond ((kv-ref #'(attribute ...) printer:)
                       => (lambda (printer)
                            (list #`(set-record-type-printer! <type> #,printer))))
                      (else '()))))))

    ;; else, type name without extra attributes
    #;
    ((_ name field ...)
     #'(define-type (name) field ...))))

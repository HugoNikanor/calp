(define-module (hnh util object)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9 gnu)
  :use-module (ice-9 curried-definitions)
  :use-module (hnh util)
  :use-module (hnh util type)
  :export (define-type
            serialize set-record-type-serializer!
            serializers
            with-serializers
            ))



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



;; Given (x type: predicate?), expand to a single `unless' form (otherwise #f)
;; Each variable gets its own unless form, to enable better error messages
(define-syntax (validator stx)
  (syntax-case stx ()
    ;; This form may be expanded both from the constructor, and from
    ;; accessor procedures.
    ;; In the constructor a different name may be used for the variable,
    ;; due to custom keyword arguments being a thing.
    ;; The field `name*' represents the variable holding the value
    ;; While the field `name' contains the true name of the field in the struct.
    ((_ name* (name kvs ...))
     (cond ((kv-ref #'(kvs ...) type:)
            => (lambda (type-stx)
                 (with-syntax ((type type-stx))
                   #'(unless (build-validator-body name* type)
                       (scm-error 'wrong-type-arg "validator"
                                  "Invalid value for `~s'. Expected ~s, got ~s"
                                  (list (quote name) (quote type) name*) #f)))))
           (else #f)))
    ((_ name) #f)))

;;; When constructing a validator from a type constructor the keyword:
;;; key in the field declaration should be honored. This means that the
;;; looked at name, and the "true name" of a field may differ.
(define-syntax (constructor-validator stx)
  (syntax-case stx ()
    ((_ (name kvs ...))
     (with-syntax ((name*
                    (cond ((kv-ref #'(kvs ...) keyword:)
                           => identity)
                          (else #'name))))
       #'(validator name* (name kvs ...))))
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

;;; Name of the created accessor
(define (accessor-name field)
  (syntax-case field ()
    ((name kvs ...)
     (cond ((kv-ref #'(kvs ...) accessor:)
            => identity)
           (else #'name)))
    (name #'name)))

;;; Name of the created lens
(define (lens-name field)
  (syntax-case field ()
    ((name kvs ...)
     (cond ((kv-ref #'(kvs ...) lens:)
            => identity)
           (else (->> (syntax->datum #'name)
                      (format #f "~a*")
                      string->symbol
                      (datum->syntax field)))))
    (name (->> (syntax->datum #'name)
               (format #f "~a*")
               string->symbol
               (datum->syntax field)))))

;; Accessors are procedures for getting and setting fields in records
(define-syntax (build-accessor stx)
  (syntax-case stx ()
    ((_ type-name (name kvs ...))
     #`(define #,(accessor-name #'(name kvs ...))
         (case-lambda ((datum)
                       ((field-get type-name name) datum))
                      ((datum new-value)
                       ;; validator uses the first field (in the list) as both
                       ;; the name of the field, and a reference to the value of
                       ;; the field. This ensures those two are the same for validator,
                       ;; while keeping name bound to the accessor in the outer scope.
                       (let ((name new-value))
                         (validator name (name kvs ...)))

                       ((field-set type-name name) datum new-value)))))

    ((_ type-name name) #'(build-accessor type-name (name)))))


(define (build-lenses stx fields)
  (map (lambda (field)
         (with-syntax ((lens* (lens-name field))
                       (accessor (accessor-name field)))
           #'(define (lens* object)
               (lambda (op)
                 (accessor object
                  (op (accessor object)))))))
       fields))


(define (get-keyword-name field)
  (syntax-case field ()
    ((name kvs ...)
     (cond ((kv-ref #'(kvs ...) keyword:)
            => identity)
           (else #'name)))
    (name #'name)))

(define (get-field-name field)
  (syntax-case field ()
    ((name _ ...) #'name)
    (name #'name)))

(define (get-field-name-and-keyword field)
  (syntax-case field ()
    ((name kvs ...)
     (cond ((kv-ref #'(kvs ...) keyword:)
            => (lambda (kv) (cons #'name kv)))
           (else (cons #'name #'name))))
    (name (cons #'name #'name))))

;; Go from my concept of field definitions, to what lambda* wants as arguments
(define (lambda*-stx field)
  (syntax-case field ()
    ((name kvs ...)
     (cond ((kv-ref #'(kvs ...) default:)
            => (lambda (dflt) #`(#,(get-keyword-name field) #,dflt)))
           (else (get-keyword-name field))))
    (name #'name)))

;; Changes a printer function to have "atomic" output.
(define (wrap-printer printer)
  (lambda (o p)
    (display
     (call-with-output-string (lambda (p_) (printer o p_)))
     p)))

;; Return a form, which when evaluated, returns the source object.
;; Compare this with "write", which outputs a string which returns the
;; source object when read back in.
;; For example `(write 'a)` would output `a`, while `(serialize 'a)`
;; would return `(quote a)`
;; A valid (but ugly) implementation of `write` would be:
;;     (define (write object port)
;;       (format port "#.~s" object))
;; assuming that the fluid `read-eval?` is set to `#t`.

(define-once serializers (make-parameter (list)))

(define (set-record-type-serializer! type-predicate serializer)
  ;; NOTE New serializers are pre-pended. This allows serializers to
  ;; be overwritten, and allows more specific serializers to be added
  ;; later. It however comes with the slight downside that `symbol?`
  ;; is one of the last serializers tested, which might make the code
  ;; slightly slower.
  (serializers (cons (cons type-predicate serializer) (serializers))))

(define (serialize object)
  (cond ((predicate-list-get (serializers) object)
         => (lambda (s) (s object)))
        ;; Assume self-quoting
        (else object)))

(set-record-type-serializer!
 symbol?
 (lambda (obj)
   (catch #t (lambda ()
               ;; A bug in Guile makes symbols which look
               ;; like floating point numbers with exponents
               ;; larger than allowed to fail to write. For
               ;; example, (string->symbol "1e500<anything>")
               ;; crashes when printed, as if `1e500` was
               ;; trying to be evaluated.
               (with-output-to-string (lambda () (write obj)))
               `(quote ,obj))
     (lambda _ `(string->symbol ,(symbol->string obj))))))

;; (set-record-type-serializer!
;;  circular-list?
;;  (lambda (obj) '(circular-lists-not-yet-supported)))


(set-record-type-serializer!
 pair?
 (lambda (pair) `(cons ,(serialize (car pair))
                  ,(serialize (cdr pair)))))

(set-record-type-serializer!
 list?
 (lambda (obj) `(list ,@(map serialize obj))))

(define-syntax with-serializers
  (syntax-rules ()
    ((_ ((pred serializer) ...)
        body ...)
     (parameterize ((serializers (cons* (cons pred serializer) ...
                                        (serializers))))
       body ...))))



(define-syntax (define-type stx)
  (syntax-case stx ()
    ((_ (name attribute ...) field ...)
     ;; These names SHOULD leak
     (with-syntax ((<type>? (construct-syntax stx #'name "~a?")))
       ;; These names are manually constructed, since generated identifiers are
       ;; only dependant on the source from which they orginate, which leads to
       ;; multiple instances of <type> being equal for similar types...
       ;; See the manual 6.10.10 Hygiene and the Top-Level
       ;; They technically shouldn't be exposed, but are as a side effect.
       (with-syntax ((<type>      (construct-syntax stx #'name "<~a>"))
                     (make-<type> (construct-syntax stx #'name "make-~a%")))
         #`(begin
             ;; Define actual type
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
                                  (constructor-validator field) ...))))
                       (else #`(lambda* (key: #,@(map lambda*-stx #'(field ...)))
                                 ;; Type validators
                                 (constructor-validator field) ...
                                 (make-<type> #,@(map get-keyword-name #'(field ...)))))))

             ;; Field accessors
             (build-accessor name field) ...

             #,@(build-lenses stx #'(field ...))

             (set-record-type-serializer!
              <type>?
              #,(cond ((kv-ref #'(attribute ...) serializer:)
                       => identity)
                      (else
                       #`(lambda (r)
                           ;; TODO instead of (<name> [key: value] ...) pairs, output
                           ;; (apply <name> (concatenate `([(key value)] ...)))
                           ;; This is a worthless extra step, but it makes pretty-print
                           ;; behave better
                           `(name
                             #,@(concatenate
                                 (map (lambda (pair)
                                        ;; We un-wrap and re-wrap field-name, since we change
                                        ;; syntax scope here
                                        (let ((field-name (syntax->datum (car pair)))
                                              (keyword (syntax->datum (cdr pair))))
                                          #`(#,(symbol->keyword keyword)
                                             ,(serialize (#,(datum->syntax stx field-name) r)))))
                                      (map get-field-name-and-keyword #'(field ...)))))))))

             ;; if printer in attribute
             (set-record-type-printer!
              ;; Wrap printer is used, since sometimes
              ;; the output port closes to early (not
              ;; sure why, tested with Guile 3.0.10,
              ;; 2025-08-28)
              <type>
              (wrap-printer
               #,(cond ((kv-ref #'(attribute ...) printer:)
                        => (lambda (printer) printer))
                       (else
                        #'(lambda (o p)
                            (->
                             (with-output-to-string
                               (lambda ()
                                 (define width
                                   (cond
                                    ((getenv "CALP_PRETTY_WIDTH")
                                     => string->number)
                                    (else
                                     (call-with-values
                                         (lambda () ((@ (vulgar info) get-terminal-size)))
                                       (lambda (_ w) w)))))
                                 (display "#.")
                                 ((@ (ice-9 pretty-print) pretty-print)
                                  (serialize o)
                                  width: (if (zero? width)
                                             ;; -2 to compensate for
                                             ;; the initial indent from the #.
                                             80 (- width 2))
                                  )))
                             (string-drop-right 1)
                             (display p)))))))))))

    ;; else, type name without extra attributes
    #;
    ((_ name field ...)
     #'(define-type (name) field ...))))

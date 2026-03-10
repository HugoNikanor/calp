(define-module (hnh util object)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9 gnu)
  :use-module (ice-9 curried-definitions)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util serialize)
  :export (define-type
            pprint-width
            record->list record->list/filtered))




;;; Width used when writing objects created by this module.
;;; 79 set as default, since that's what pretty-print defaults to also
(define-once pprint-width
  (make-parameter 79))



;; If given a syntax list extract the first lexeme, if given a "symbol", return that.
(define (syntax-first stx)
  (syntax-case stx ()
    ((a _ ...) #'a)
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
    ;; This form may be expanded both from:
    ;; - the constructor, and
    ;; - the accessor procedures.
    ;; In the constructor a different name may be used for the variable,
    ;; due to custom keyword arguments being a thing.
    ;; The field `name*' represents the variable holding the value
    ;; While the field `name' contains the true name of the field in the struct.
    ((_ name* (name kvs ...))
     (cond ((kv-ref #'(kvs ...) type:)
            => (lambda (type-stx)
                 (with-syntax ((type type-stx))
                   #'(typecheck name* type (symbol->string (quote name))))))
           (else #f)))
    ((_ name) #f)))

;;; When constructing a validator from a type constructor the keyword:
;;; key in the field declaration should be honored. This means that the
;;; looked at name, and the "true name" of a field may differ.
(define-syntax (constructor-validator stx)
  (syntax-case stx ()
    ((_ (name kvs ...))
     (with-syntax ((name* (get-keyword-name #'(name kvs ...))))
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
     (or (kv-ref #'(kvs ...) accessor:) #'name))
    (name #'name)))

;;; Name of the created lens
(define (lens-name field)
  (syntax-case field ()
    ((name kvs ...)
     (or (kv-ref #'(kvs ...) lens:)
         (->> (syntax->datum #'name)
              (format #f "~a*")
              string->symbol
              (datum->syntax field))))
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
     (or (kv-ref #'(kvs ...) keyword:) #'name))
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



(define-once object-to-rtd (list))

(define (set-object-to-rtd! type-predicate rtd)
  (set! object-to-rtd (cons (cons type-predicate rtd) object-to-rtd)))

;;; NOTE Consider the option of allowing the user (of the library) to
;;; cache the result of find-rtd, instead of having to search for it each
;;; time record->list is used.

(define (find-rtd record)
  (or (predicate-list-get object-to-rtd record)
      (scm-error
       'misc-error "find-rtd"
       "Record appears to not be created through (@ (hnh util object) define-type): ~s"
       (list record) #f)))

(define (record->list proc record)
  (let ((rtd (find-rtd record)))
    (map (lambda (field) (proc field ((record-accessor rtd field) record)))
         (record-type-fields rtd))))

(define (record->list/filtered proc record)
  (filter identity (record->list proc record)))



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
                                (lambda #,(map get-keyword-name #'(field ...))
                                  (constructor-validator field) ...))))
                       (else #`(lambda* (key: #,@(map lambda*-stx #'(field ...)))
                                 ;; Type validators
                                 (constructor-validator field) ...
                                 (make-<type> #,@(map get-keyword-name #'(field ...)))))))

             ;; Field accessors
             (build-accessor name field) ...

             ;; Field lenses
             #,@(build-lenses stx #'(field ...))


             ;; Serializer
             (set-record-type-serializer!
              <type>?
              #,(or (kv-ref #'(attribute ...) serializer:)
                    #`(lambda (r)
                        `(name
                          #,@(concatenate
                              (map (lambda (pair)
                                     ;; We un-wrap and re-wrap field-name, since we change
                                     ;; syntax scope here
                                     (let ((field-name (syntax->datum (car pair)))
                                           (keyword (syntax->datum (cdr pair))))
                                       #`(#,(symbol->keyword keyword)
                                          ,(serialize (#,(datum->syntax stx field-name) r)))))
                                   (map get-field-name-and-keyword #'(field ...))))))))

             ;; Record type declaration index
             (set-object-to-rtd! <type>? <type>)

             ;; Printer
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
                                 (display "#.")
                                 ((@ (hnh util pprint) pretty-print)
                                  (serialize o)
                                  width: (pprint-width))))
                             (string-drop-right 1)
                             (display p)))))))))))

    ;; else, type name without extra attributes
    #;
    ((_ name field ...)
     #'(define-type (name) field ...))))

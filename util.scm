(define-module (util)
  #:use-module (srfi srfi-1)
  #:export (destructure-lambda let-multi fold-lists catch-let
                               for-each-in
                               define-quick-record define-quick-record!)
  #:replace (let*)
  )

(define-public upstring->symbol (compose string->symbol string-upcase))

(define-public symbol-upcase (compose string->symbol string-upcase symbol->string))

(define-public symbol-downcase (compose string->symbol string-downcase symbol->string))
       
(define-syntax destructure-lambda
  (syntax-rules ()
    ((_ expr-list body ...)
     (lambda (expr)
       (apply (lambda expr-list body ...) expr)))))

(define-syntax let-multi
  (syntax-rules ()
      ((let-m identifiers lst body ...)
       (apply (lambda identifiers body ...)
              lst))))

(define-syntax fold-lists
  (syntax-rules (lambda)
    ((_ (lambda ((list-part ...) object) body ...) seed list)
     (fold (lambda (kv object)
             (let-multi (list-part ...) kv
                        body ...))
           seed
           list))))

(define-syntax catch-let
  (syntax-rules ()
    ((_ thunk ((type handler) ...))
     (catch #t thunk
       (lambda (err . args)
         (case err
           ((type) (apply handler err args)) ...
           (else (format #t "Unhandled error type ~a, rethrowing ~%" err)
                 (apply throw err args))))))))

(define-syntax-rule (for-each-in lst proc)
  (for-each proc lst))


(define (class-name symb) (symbol-append '< symb '>))
(define (constructor symb) (symbol-append 'make- symb))
(define (pred symb) (symbol-append symb '?))

(define (getter name symb) (symbol-append 'get- name '- symb))
(define* (setter name symb #:optional bang?)
  (symbol-append 'set- name '- symb (if bang? '! (symbol))))

(define (%define-quick-record internal-define bang? name fields)
  (let ((symb (gensym)))
    `(begin (,internal-define ,(class-name name)
             (,(constructor name) ,@fields)
             ,(pred name)
             ,@(map (lambda (f) `(,f ,(getter f symb) ,(setter f symb bang?)))
                    fields))
            ,@(map (lambda (f) `(define ,f (make-procedure-with-setter
                                       ,(getter f symb) ,(setter f symb bang?))))
                   fields))))

(define-macro (define-quick-record name . fields)
  (%define-quick-record '(@ (srfi srfi-9 gnu) define-immutable-record-type)
                        #f name fields))

(define-macro (define-quick-record! name . fields)
  (%define-quick-record '(@ (srfi srfi-9) define-record-type)
                        #t name fields))

(define-syntax let*
  (syntax-rules ()

    ;; Base case
    [(_ () body ...)
     (begin body ...)]

    ;; (let (((a b) '(1 2))) (list b a)) => (2 1)
    [(_ (((k k* ...) list-value) rest ...)
        body ...)
     (apply (lambda (k k* ...)
              (let* (rest ...)
                    body ...))
            list-value)]

    ;; "Regular" case
    [(_ ((k value) rest ...) body ...)
     (let ((k value))
       (let* (rest ...)
        body ...))]))

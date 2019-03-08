(define-module (util)
  #:use-module (srfi srfi-1)
  #:export (destructure-lambda let-multi fold-lists catch-let
                               for-each-in)
  )

(define-public upstring->symbol (compose string->symbol string-upcase))

(define-public symbol-upcase (compose string->symbol string-upcase symbol->string))

(define-public symbol-downcase (compose string->symbol string-downcase symbol->string))
       
(define-syntax destructure-lambda
  (syntax-rules ()
    ((_ expr-list body ...)
     (lambda (expr)
       (apply (lambda expr-list body ...) expr)))))

#; 
(map (destructure-lambda (a b) (+ a b))
     (map list (iota 10) (iota 10 10)))

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

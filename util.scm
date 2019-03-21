(define-module (util)
  #:use-module (srfi srfi-1)
  #:export (destructure-lambda let-multi fold-lists catch-let
                               for-each-in
                               define-quick-record define-quick-record!
                               mod! sort* sort*!)
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

(define-syntax catch-let
  (syntax-rules ()
    ((_ thunk ((type handler) ...))
     (catch #t thunk
       (lambda (err . args)
         (case err
           ((type) (apply handler err args)) ...
           (else (format #t "Unhandled error type ~a, rethrowing ~%" err)
                 (apply throw err args))))))))

;;; For-each with arguments in reverse order.
(define-syntax-rule (for-each-in lst proc)
  (for-each proc lst))


;;; Helper macros to make define-quick-record better

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

;;; Creates srfi-9 define{-immutable,}-record-type declations.
;;; Also creates srfi-17 accessor ((set! (access field) value))

(define-macro (define-quick-record name . fields)
  (%define-quick-record '(@ (srfi srfi-9 gnu) define-immutable-record-type)
                        #f name fields))

(define-macro (define-quick-record! name . fields)
  (%define-quick-record '(@ (srfi srfi-9) define-record-type)
                        #t name fields))

;;; Replace let* with a version that can bind from lists.
;;; Also supports SRFI-71 (extended let-syntax for multiple values)
;;; Example:
;; (let* ([a b (values 1 2)]               ; SRFI-71
;;        [(c d) '(3 4)]                   ; Let-list (mine)
;;        [e 5])                           ; Regular
;;   (list e d c b a))
;; ;; => (5 4 3 2 1)
;;;

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
         body ...))]

    ;; SRFI-71 let-values
    [(_ ((k k* ... values) rest ...) body ...)
     (call-with-values (lambda () values)
       (lambda (k k* ...)
         (let* (rest ...)
           body ...)))]

    ))

;; Like set!, but applies a transformer on the already present value.
(define-syntax-rule (mod! field transform-proc)
  (set! field (transform-proc field)))

(define-public (concat lists)
  (apply append lists))

;;; This function borrowed from web-ics (calendar util)
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

;;; This function borrowed from web-ics (calendar util)
(define* (sort*! items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort! items (lambda (a b)
                 (comperator (get a)
                             (get b)))))

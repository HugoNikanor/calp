(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module ((ice-9 optargs) #:select (define*-public))
  #:use-module ((sxml fold) #:select (fold-values))
  #:export (destructure-lambda let-multi fold-lists catch-let
                               for-each-in for
                               define-quick-record
                               mod! sort* sort*!
                               find-min
                               catch-multiple)
  #:replace (let* set!)
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

(define-syntax for
  (syntax-rules (in)
    ((for <var> in <collection> b1 body ...)
     (for-each (lambda (<var>) b1 body ...)
               <collection>))))



;;; Helper macros to make define-quick-record better

(define (class-name symb) (symbol-append '< symb '>))
(define (constructor symb) (symbol-append 'make- symb))
(define (pred symb) (symbol-append symb '?))

(define (getter name symb) (symbol-append 'get- name '- symb))
(define* (setter name symb #:optional bang?)
  (symbol-append 'set- name '- symb (if bang? '! (symbol))))

(define (%define-quick-record internal-define bang? name fields)
  (let ((symb (gensym)))
    `((,internal-define ,(class-name name)
             (,(constructor name) ,@fields)
             ,(pred name)
             ,@(map (lambda (f) `(,f ,(getter f symb) ,(setter f symb bang?)))
                    fields))
            ,@(map (lambda (f) `(define ,f (make-procedure-with-setter
                                       ,(getter f symb) ,(setter f symb bang?))))
                   fields))))

;;; Creates srfi-9 define{-immutable,}-record-type declations.
;;; Also creates srfi-17 accessor ((set! (access field) value))

;;; TODO allow extra properties to be sent to this macro,
;;; such as @var{:muttable} or @var{:immutable}

(define-macro (define-quick-record name . fields)
  (let ((public-fields  (or (assoc-ref fields #:public)  '()))
        (private-fields (or (assoc-ref fields #:private) '())))
    `(begin
       ,@(%define-quick-record '(@ (srfi srfi-9 gnu) define-immutable-record-type)
                               #f name
                               (append public-fields private-fields))
       ,@(map (lambda (field) `(export ,field))
              public-fields))))



;; Replace let* with a version that can bind from lists.
;; Also supports SRFI-71 (extended let-syntax for multiple values)
;; @lisp
;; (let* ([a b (values 1 2)]               ; @r{SRFI-71}
;;        [(c d) '(3 4)]                   ; @r{Let-list (mine)}
;;        [(a b . c) (cons* 1 2 3)]        ; @r{Improper list matching (mine)}
;;        [e 5])                           ; @r{Regular}
;;   (list e d c b a))
;; ;; => (5 4 3 2 1)
;; @end lisp
(define-syntax let*
  (syntax-rules ()

    ;; Base case
    [(_ () body ...)
     (begin body ...)]

    ;; (let (((a b) '(1 2))) (list b a)) => (2 1)
    [(_ (((k ... . (k*)) list-value) rest ...)
        body ...)
     (apply (lambda (k ... k*)
              (let* (rest ...)
                body ...))
            list-value)]

    ;; Improper list matching
    ;; (let* (((a b . c) (cons* 1 2 3))) (list a c)) ; => (1 3)
    [(_ (((k1 k ... . k*) imp-list) rest ...)
        body ...)
     (apply (lambda (k1 k ... k*)
              (let* (rest ...)
                body ...))
            (improper->proper-list
             imp-list (length (quote (k1 k ...)))))]

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

    ;; Declare variable without a value (actuall #f).
    ;; Useful for inner mutation.
    [(_ (v rest ...) body ...)
     (let* ((v #f) rest ...) body ...)]
    ))

(define (improper->proper-list lst len)
  (let* ((head tail (split-at lst len)))
    (append head (list tail))))



;; Allow set to work on multiple values at once,
;; similar to Common Lisp's @var{setf}
;; @example
;; (set! x 10
;;       y 20)
;; @end example
;; Still requires all variables to be defined beforehand.
(define-syntax set!
  (syntax-rules ()
    ((_ field val)
     ((@ (guile) set!) field val))
    ((_ field val rest ...)
     (begin ((@ (guile) set!) field val)
            (set! rest ...)))))

;; Like set!, but applies a transformer on the already present value.
(define-syntax mod!
  (syntax-rules ()
    ((_ field proc)
     (set! field (proc field)))
    ((_ field transform-proc rest ...)
     (begin (set! field (transform-proc field))
            (mod! rest ...)))))

(define-public (concat lists)
  (apply append lists))

;; This function borrowed from web-ics (calendar util)
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

;; Sorts the list @var{items}. @em{May} destroy the input list in the process
(define* (sort*! items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort! items (lambda (a b)
                 (comperator (get a)
                             (get b)))))

;; Finds the smallest element in @var{items}, compared with @var{<} after
;; applying @var{foo}. Returns 2 values. The smallest item in @var{items},
;; and the other items in some order.
(define (find-min < ac items)
  (if (null? items)
      ;; Vad fan retunerar man här?
      (values #f '())
      (fold-values
       (lambda (c min other)
         (if (< (ac c) (ac min))
             ;; Current stream head is smaller that previous min
             (values c (cons min other))
             ;; Previous min is still smallest
             (values min (cons c other))))
       (cdr items)
       ;; seeds:
       (car items) '())))

;; TODO This might be utterly broken, do some real tests on it
;; (and then run equivalent tests on the stream variant)
(define-public (filter-sorted proc list)
  (take-while
   proc (drop-while
         (negate proc) list)))

;; (define (!= a b) (not (= a b)))
(define-public != (negate =))

(define-public (take-to lst i)
  "Like @var{take}, but might lists shorter than length."
  (if (> i (length lst))
      lst (take lst i)))

(define-public (as-string s)
  (if (symbol? s) (symbol->string s) s))

(define-public (as-symb s)
  (if (string? s) (string->symbol s) s))

(define-public (enumerate lst)
  (zip (iota (length lst))
       lst))

;; Map with index
(define-syntax-rule (map-each proc lst)
  (map (lambda (x i) (proc x i))
       lst (iota (length lst))))

(export map-each)

;; Takes a procedure returning multiple values, and returns a function which
;; takes the same arguments as the original procedure, but only returns one of
;; the procedures. Which procedure can be sent as an additional parameter.
(define*-public (unval proc #:optional (n 0))
  (lambda args
    (call-with-values (lambda () (apply proc args))
      (lambda args (list-ref args n)))))

;; Takes a (non nested) list, and replaces all single underscore
;; symbols with a generated symbol. For macro usage.
(define (multiple-ignore lst)
  (cond ((not-pair? lst) lst)
        ((eq? '_ (car lst)) (cons (gensym "ignored_")
                                  (multiple-ignore (cdr lst))))
        (else (cons (car lst)
                    (multiple-ignore (cdr lst))))))

(define (catch-recur% errs thunk cases)
  (let* ((v (car errs))
         (case other (partition (lambda (case) (eq? v (car case))) cases))
         (g!rest (gensym "rest")))
    `(catch (quote ,v)
       ,(if (null? (cdr errs))
            thunk
            `(lambda () ,(catch-recur% (cdr errs) thunk other)))
       (lambda (err . ,g!rest)
         (apply (lambda ,(multiple-ignore (second (car case)))
                  ,@(cddr (car case)))
                ,g!rest)))))

;; Like @var{catch}, but multiple handlers can be specified.
;; Each handler is on the form
;; @example
;; [err-symb (args ...) body ...]
;; @end example
;; 
;; Only errors with a handler are caught. Error can *not* be given as
;; an early argument.
(define-macro (catch-multiple thunk . cases)
  (catch-recur% (map car cases) thunk cases))


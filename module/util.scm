(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module ((ice-9 optargs) #:select (define*-public))
  #:use-module ((sxml fold) #:select (fold-values))
  #:use-module (srfi srfi-9 gnu)
  #:re-export (define*-public)
  #:export (for define-quick-record
                mod! sort* sort*!
                mod/r! set/r!
                find-min
                catch-multiple
                quote?
                re-export-modules
                use-modules*
                -> set set-> aif awhen
                tree-map let-lazy let-env
                case* define-many
                and=>>
                )
  #:replace (let* set! define-syntax
                  when unless if))

((@ (guile) define-syntax) define-syntax
  (syntax-rules ()
    ((_ (name args ...) body ...)
     ((@ (guile) define-syntax) name
      (lambda (args ...)
        body ...)))
    ((_ otherwise ...)
     ((@ (guile) define-syntax) otherwise ...))))

(define-public *unspecified* ((@ (guile) if) #f #f))



(define-syntax-rule (when pred body ...)
  (if pred (begin body ...) '()))

(define-syntax-rule (unless pred body ...)
  (if pred '() (begin body ...)))

(define-syntax if
  (syntax-rules ()
   [(_ p t)
    (when p t)]

   [(_ p t f ...)
    ((@ (guile) if) p t
     (begin f ...))]))


(define-syntax aif
  (lambda (stx)
    (syntax-case stx ()
      [(_ condition true-clause false-clause)
       (with-syntax ((it (datum->syntax stx 'it)))
         #'(let ((it condition))
             (if it true-clause false-clause)))])))

(define-syntax awhen
  (lambda (stx)
    (syntax-case stx ()
      [(_ condition body ...)
       (with-syntax ((it (datum->syntax stx 'it)))
         #'(let ((it condition))
             (when it body ...)))])))

#;
(define-macro (awhen pred . body)
  `(let ((it ,pred))
     (when it
       ,@body)))



(define-public upstring->symbol (compose string->symbol string-upcase))

(define-public symbol-upcase (compose string->symbol string-upcase symbol->string))

(define-public symbol-downcase (compose string->symbol string-downcase symbol->string))

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
        (private-fields (or (assoc-ref fields #:private) '()))
        (printer (and=> (assoc-ref fields #:printer) car)))
    `(begin
       ,@(%define-quick-record '(@ (srfi srfi-9 gnu) define-immutable-record-type)
                               #f name
                               (append public-fields private-fields))
       (when ,printer
         ((@ (srfi srfi-9 gnu) set-record-type-printer!)
          ,(class-name name) ,printer))
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



(define-public (swap f)
  (lambda args (apply f (reverse args))))

;; Like `case', but evals the case parameters
(define-syntax case*
  (syntax-rules (else)
    [(_ invalue ((value ...) body ...) ...)
     (cond ((memv invalue (list value ...))
            body ...)
           ...)]))

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

(define-syntax modf%
  (syntax-rules (=)
    ((_ field = (op args ...))
     (set! field (op field args ...)))
    ((_ field proc)
     (set! field (proc field))))  )


;; Like set!, but applies a transformer on the already present value.
(define-syntax mod!
  (syntax-rules (=)
    ((_) *unspecified*)
    ((_ field = proc)
     (modf% field = proc))

    ((_ field = proc rest ...)
     (begin (modf% field = proc) (mod! rest ...)))

    ((_ field proc)
     (modf% field proc))

    ((_ field proc rest ...)
     (begin (modf% field proc) (mod! rest ...)))))

(define-syntax-rule (set/r! args ... final)
  (let ((val final))
    (set! args ... val)
    val))

(define-syntax mod/r!
  (syntax-rules (=)
    ((_ args ... field = (proc pargs ...))
     (begin (mod! args ...)
            (set/r! field (proc field pargs ...))))
    ((_  args ... ffield fproc)
     (begin (mod! args ...)
            (set/r! ffield (fproc ffield))))))


(define-syntax define-many
  (syntax-rules ()
    [(_) (begin)]
    [(_ def) (begin)]
    [(_ (symbols ...) value rest ...)
     (begin (define symbols value) ...
            (define-many rest ...))]
    [(_ def (symbols ...) value rest ...)
     (begin (def symbols value) ...
            (define-many def rest ...))]))

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

(define-public (flatten lst)
  (fold (lambda (subl done)
          (append done ((if (list? subl) flatten list) subl)))
        '() lst))

(define-syntax let-lazy
  (syntax-rules ()
    [(_ ((field value) ...)
        body ...)
     (let ((field (delay value)) ...)
       (let-syntax ((field (identifier-syntax (force field))) ...)
         body ...))]))

(define-public (map/dotted proc dotted-list)
  (cond ((null? dotted-list) '())
        ((not-pair? dotted-list) (proc dotted-list))
        (else
         (cons (proc (car dotted-list))
               (map/dotted proc (cdr dotted-list))))))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(define-public (assq-merge a b)
  (fold (lambda (entry alist)
          (let* (((k . v) entry)
                 (o (assq-ref alist k)))
            (assq-set! alist k (append v (or o '())))))
        (copy-tree a) b))


(define-macro (use-modules* . forms)
  `(use-modules
    ,@(concatenate
       (map (lambda (form)
              (map (lambda (sub) (list (car form) sub))
                   (cadr form)))
            forms))))



(define-syntax ->
  (syntax-rules ()
    [(-> obj) obj]
    [(-> obj (func args ...) rest ...)
     (-> (func obj args ...) rest ...)]
    [(-> obj func rest ...)
     (-> (func obj) rest ...)]))

;; Non-destructive set, syntax extension from set-fields from (srfi
;; srfi-9 gnu). Also doubles as a non-destructive mod!, if the `='
;; operator is used.
(define-syntax set
  (syntax-rules (=)
    [(set (acc obj) value)
     (set-fields
      obj ((acc) value))]
    [(set (acc obj) = (op rest ...))
     (set-fields
      obj ((acc) (op (acc obj) rest ...)))]))

(define-syntax set->
  (syntax-rules (=)
    [(_ obj) obj]
    [(_ obj (func = (op args ...)) rest ...)
     (set-> (set (func obj) (op (func obj) args ...)) rest ...)]
    [(_ obj (func args ...) rest ...)
     (set-> (set (func obj) args ...) rest ...)]))

(define-syntax and=>>
  (syntax-rules ()
    [(_ value) value]
    [(_ value proc rest ...)
     (and=>> (and=> value proc)
             rest ...)]))



(define-syntax let-env
  (syntax-rules ()
    [(_ ((name value) ...)
        body ...)

     (let ((env-pairs
            (map (lambda (n new-value)
                   (list n new-value (getenv n)))
                 (list (symbol->string (quote name)) ...)
                 (list value ...))))
       (for-each (lambda (pair) (setenv (car pair) (cadr pair)))
                 env-pairs)
       (let ((return (begin body ...)))
        (for-each (lambda (pair) (setenv (car pair) (caddr pair)))
                  env-pairs)
        return))]))

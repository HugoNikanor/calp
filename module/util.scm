(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-88)           ; postfix keywords
  #:use-module ((ice-9 optargs) #:select (define*-public))
  #:use-module ((sxml fold) #:select (fold-values))
  #:use-module ((srfi srfi-9 gnu) #:select (set-fields))
  #:re-export (define*-public)
  #:export (for mod! sort* sort*!
                mod/r! set/r!
                find-min
                catch-multiple
                quote?
                re-export-modules
                -> ->> set set-> aif awhen
                tree-map let-lazy let-env
                case* define-many
                and=>> label
                print-and-return
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



;; NOTE
;; Instead of returning the empty list a better default value
;; for when and unless would be the identity element for the
;; current context.
;; So (string-append (when #f ...)) would expand into
;; (string-append (if #f ... "")).
;; This however requires type interferance, which i don't
;; *currently* have.

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


(define-syntax (aif stx)
  (syntax-case stx ()
    [(_ condition true-clause false-clause)
     (with-syntax ((it (datum->syntax stx 'it)))
       #'(let ((it condition))
           (if it true-clause false-clause)))]))

(define-syntax (awhen stx)
  (syntax-case stx ()
    [(_ condition body ...)
     (with-syntax ((it (datum->syntax stx 'it)))
       #'(let ((it condition))
           (when it body ...)))]))

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
    ((for (<var> <vars> ...) in <collection> b1 body ...)
     (map ((@ (ice-9 match) match-lambda) [(<var> <vars> ...) b1 body ...])
          <collection>))
    ((for <var> in <collection> b1 body ...)
     (map (lambda (<var>) b1 body ...)
          <collection>))))



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



(define-macro (print-and-return expr)
  (let ((str (gensym "str"))
        (result (gensym "result")))
    `(let* ((,result ,expr)
            (,str (format #f "~a [~a]~%" ,result (quote ,expr))))
       (display ,str (current-error-port))
       ,result)))



(define-public (swap f)
  (lambda args (apply f (reverse args))))


(define-syntax case*%
  (syntax-rules (else)
    [(_ _ else)
     #t]
    [(_ invalue (value ...))
     (memv invalue (list value ...))]
    #;
    [(_ invalue target)
     (eq? invalue target)]))

;; Like `case', but evals the case parameters
(define-syntax case*
  (syntax-rules (else)
    [(_ invalue (cases body ...) ...)
     (cond ((case*% invalue cases)
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

;; Attach a label to a function, allowing it to call itself
;; without actually giving it a name (can also be thought
;; of as letrec-1).
;; @example
;; ((label fact
;;   (match-lambda
;;     [0 1]
;;     [x (* x (fact (1- x)))]))
;;  5)
;; @end example
(define-syntax label
  (syntax-rules ()
    [(_ self proc)
     (letrec ((self proc))
       proc)]))


;; This function borrowed from web-ics (calendar util)
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

;; Sorts the list @var{items}. @emph{May} destroy the input list in the process
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

(define-public (string-take-to str i)
  (if (> i (string-length str))
      str (string-take str i)))

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

;; Merges two association lists, comparing with eq.
;; The cdrs in all pairs in both lists should be lists,
;; If a key is present in both then the contents of b is
;; put @emph{before} the contents in a.
;; @example
;; (assq-merge '((k 1)) '((k 2)))
;; => ((k 2 1))
;; @end example
(define-public (assq-merge a b)
  (fold (lambda (entry alist)
          (let* (((k . v) entry)
                 (o (assq-ref alist k)))
            (assq-set! alist k (append v (or o '())))))
        (copy-tree a) b))

(define*-public (assq-limit alist optional: (number 1))
  (map (lambda (pair)
         (take-to pair (1+ number)))
       alist))

(define-public (group-by proc lst)
  (let ((h (make-hash-table)))
    (for value in lst
         (let ((key (proc value)))
           (hash-set! h key (cons value (hash-ref h key '())))))
    (hash-map->list list h)))

;; Returns the cross product between l1 and l2.
;; each element is a cons cell.
(define (cross-product% l1 l2)
  (concatenate
   (map (lambda (a)
          (map (lambda (b) (cons a b))
               l2))
        l1)))

(define-public (cross-product . args)
  (if (null? args)
      '()
      (let* ((last rest (car+cdr (reverse args))))
        (reduce-right cross-product% '()
                      (reverse (cons (map list last) rest ))))))

;; Given an arbitary tree, do a pre-order traversal, appending all strings.
;; non-strings allso allowed, converted to strings and also appended.
(define-public (string-flatten tree)
  (cond [(string? tree) tree]
        [(list? tree) (string-concatenate (map string-flatten tree))]
        [else (format #f "~a" tree)]))

(define-public (intersperce item list)
  (let loop ((flipflop #f)
             (rem list))
    (if (null? rem)
        '()
        (if flipflop
            (cons item (loop (not flipflop) rem))
            (cons (car rem) (loop (not flipflop) (cdr rem)))
            ))))



(define-syntax ->
  (syntax-rules ()
    [(-> obj) obj]
    [(-> obj (func args ...) rest ...)
     (-> (func obj args ...) rest ...)]
    [(-> obj func rest ...)
     (-> (func obj) rest ...)]))

(define-syntax ->>
  (syntax-rules ()
    ((->> obj)
     obj)
    ((->> obj (func args ...) rest ...)
     (->> (func args ... obj) rest ...))
    ((->> obj func rest ...)
     (->> (func obj) rest ...))))

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

(define-public (group list width)
  (unless (null? list)
    (let* ((row rest (split-at list width)))
      (cons row (group rest width)))))

;; repeatedly apply proc to base unitl @var{until} is satisfied.
;; (a → a), (a → bool), a → a
(define-public (iterate proc until base)
  (let loop ((o base))
    (if (until o)
        o
        (loop (proc o)))))

;; (a → values a), list ... → values a
(define-public (valued-map proc . lists)
  (apply values
   (apply append-map
          (lambda args
            (call-with-values (lambda () (apply proc args)) list))
          lists)))




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

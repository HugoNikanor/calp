(define-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)           ; postfix keywords
  :use-module ((sxml fold) :select (fold-values))
  :use-module ((srfi srfi-9 gnu) :select (set-fields))
  :use-module ((ice-9 copy-tree) :select (copy-tree))
  :use-module ((ice-9 control) :select (call/ec))
  :re-export (fold-values)
  :export (aif
           awhen
           for
           begin1
           print-and-return
           swap
           set/r!
           label
           sort* sort*!
           find-extreme find-min find-max
           filter-sorted
           !=
           take-to
           string-take-to
           string-first
           string-last
           as-symb
           enumerate
           unval
           flatten
           let-lazy
           map/dotted

           assq-merge
           kvlist->assq
           assq-limit

           group-by
           split-by

           span-upto
           cross-product

           string-flatten
           intersperse
           insert-ordered

           -> ->>
           set set->
           and=>>

           downcase-symbol
           group
           iterate
           valued-map

           assoc-ref-all
           assq-ref-all
           assv-ref-all

           uniqx
           uniq
           univ
           uniqv
           unique

           vector-last

           ->string

           catch*
           )
  :replace (set! define-syntax
                 when unless))

((@ (guile) define-syntax) define-syntax
  (syntax-rules ()
    ((_ (name args ...) body ...)
     ((@ (guile) define-syntax) name
      (lambda (args ...)
        body ...)))
    ((_ otherwise ...)
     ((@ (guile) define-syntax) otherwise ...))))



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



(define-syntax (for stx)
  (syntax-case stx (in)
    ((for (<var> <vars> ...) in <collection> b1 body ...)
     (with-syntax ((break (datum->syntax stx 'break))
                   (continue (datum->syntax stx 'continue)))
       #'(call/ec
          (lambda (break)
            (map ((@ (ice-9 match) match-lambda)
                  [(<var> <vars> ...)
                   (call/ec
                    (lambda (raw-continue)
                      (let ((continue
                             (case-lambda
                               (() #f)
                               (args (apply raw-continue args)))))
                        b1 body ...)))])
                 <collection>)))))

    ((for (<var> <vars> ... . <tail>) in <collection> b1 body ...)
     #'(call/ec
        (lambda (break)
          (map ((@ (ice-9 match) match-lambda)
                [(<var> <vars> ... . <tail>)
                 (call/ec
                  (lambda (raw-continue)
                    (let ((continue
                           (case-lambda
                             (() #f)
                             (args (apply raw-continue args)))))
                      b1 body ...)))])
               <collection>))))
    ((for <var> in <collection> b1 body ...)
     (with-syntax ((break (datum->syntax stx 'break))
                   (continue (datum->syntax stx 'continue)))
       #'(call/ec
          (lambda (break)
            (map (lambda (<var>)
                   (call/ec (lambda (raw-continue)
                              (let ((continue
                                     (case-lambda
                                       (() #f)
                                       (args (apply raw-continue args)))))
                                b1 body ...))))
                 <collection>)))))))



(define-syntax-rule (begin1 first rest ...)
  (call-with-values (lambda () first)
    (lambda returned
      rest ...
      (apply values returned))))





(define-syntax-rule (print-and-return expr)
  (let ((result expr))
    (display (format #f "~a [~a]~%" result (quote expr))
             (current-error-port))
    result))




(define (swap f)
  (lambda args (apply f (reverse args))))


;; Allow set to work on multiple values at once,
;; similar to Common Lisp's @var{setf}
;; @example
;; (set! x 10
;;       y 20)
;; @end example
;; Still requires all variables to be defined beforehand.
(define-syntax set!
  (syntax-rules (=)
    ((_ field = (op args ...) rest ...)
     (set! field (op field args ...)
           rest ...))
    ((_ field = proc rest ...)
     (set! field (proc field)
           rest ...))
    ((_ field val)
     ((@ (guile) set!) field val))
    ((_ field val rest ...)
     (begin ((@ (guile) set!) field val)
            (set! rest ...)))))

;; only evaluates the final form once
(define-syntax set/r!
  (syntax-rules (=)
    ((_ args ... v = something)
     (begin
       (set! args ... v = something)
       v))
    ((_ args ... final)
     (let ((val final))
       (set! args ... val)
       val))))


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
(define* (sort* items comperator optional: (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

;; Sorts the list @var{items}. @emph{May} destroy the input list in the process
(define* (sort*! items comperator optional: (get identity))
  "A sort function more in line with how python's sorted works"
  (sort! items (lambda (a b)
                 (comperator (get a)
                             (get b)))))

;; Given {items, <} finds the most extreme value.
;; Returns 2 values. The extremest item in @var{items},
;; and the other items in some order.
;; Ord b => (list a) [, (b, b -> bool), (a -> b)] -> a, (list a)
(define* (find-extreme items optional: (< <) (access identity))
  (when (null? items)
    (scm-error 'wrong-type-arg "find-extreme"
               "Can't find extreme in an empty list"
               #f #f))
  (fold-values
   (lambda (c min other)
     (if (< (access c) (access min))
         ;; Current stream head is smaller that previous min
         (values c (cons min other))
         ;; Previous min is still smallest
         (values min (cons c other))))
   (cdr items)
   ;; seeds:
   (car items) '()))

(define* (find-min list optional: (access identity))
  (find-extreme list < access))

(define* (find-max list optional: (access identity))
  (find-extreme list > access))

(define (filter-sorted proc list)
  (take-while
   proc (drop-while
         (negate proc) list)))

;; (define (!= a b) (not (= a b)))
(define != (negate =))

(define (take-to lst i)
  "Like @var{take}, but might lists shorter than length."
  (if (> i (length lst))
      lst (take lst i)))

(define (string-take-to str i)
  (if (> i (string-length str))
      str (string-take str i)))

(define (string-first str)
  (string-ref str 0))

(define (string-last str)
  (string-ref str (1- (string-length str))))

(define (as-symb s)
  (if (string? s) (string->symbol s) s))

(define (enumerate lst)
  (zip (iota (length lst))
       lst))

;; Takes a procedure returning multiple values, and returns a procedure which
;; takes the same arguments as the original procedure, but only returns one of
;; the return values. Which value to return can be sent as an additional parameter.
(define* (unval proc optional: (n 0))
  (lambda args
    (call-with-values (lambda () (apply proc args))
      (lambda args (list-ref args n)))))

(define (flatten lst)
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

(define (map/dotted proc dotted-list)
  (cond ((null? dotted-list) '())
        ((not-pair? dotted-list) (proc dotted-list))
        (else
         (cons (proc (car dotted-list))
               (map/dotted proc (cdr dotted-list))))))

;; Merges two association lists, comparing with eq.
;; The cdrs in all pairs in both lists should be lists,
;; If a key is present in both then the contents of b is
;; put @emph{before} the contents in a.
;; @example
;; (assq-merge '((k 1)) '((k 2)))
;; => ((k 2 1))
;; @end example
(define (assq-merge a b)
  (fold (lambda (entry alist)
          (let* ((k v (car+cdr entry))
                 (o (assq-ref alist k)))
            (assq-set! alist k (append v (or o '())))))
        (copy-tree a) b))

(define (kvlist->assq kvlist)
  (map (lambda (pair)
         (cons (keyword->symbol (car pair))
               (cadr pair)))
       (group kvlist 2)))

(define* (assq-limit alist optional: (number 1))
  (map (lambda (pair)
         (take-to pair (1+ number)))
       alist))

(define (group-by proc lst)
  (let ((h (make-hash-table)))
    (for value in lst
         (let ((key (proc value)))
           (hash-set! h key (cons value (hash-ref h key '())))))
    (hash-map->list cons h)))

;; (split-by '(0 1 2 3 4 2 5 6) 2)
;; ⇒ ((0 1) (3 4) (5 6))
(define (split-by list item)
  (let loop ((done '())
             (current '())
             (rem list))
    (cond [(null? rem)
           (reverse (cons (reverse current) done))]
          [(eqv? item (car rem))
           (loop (cons (reverse current) done)
                 '()
                 (cdr rem))]
          [else
           (loop done
                 (cons (car rem) current)
                 (cdr rem))])))



;; Simar to span from srfi-1, but never takes more than
;; @var{count} items. Can however still take less.
;; @example
;; (span-upto 2 char-numeric? (string->list "123456"))
;; ⇒ (#\1 #\2)
;; ⇒ (#\3 #\4 #\5 #\6)
;; (span-upto 2 char-numeric? (string->list "H123456"))
;; ⇒ ()
;; ⇒ (#\H #\1 #\2 #\3 #\4 #\5 #\6)
;; @end example
(define (span-upto count predicate list)
  (let loop ((remaining count)
             (taken '())
             (list list))
    (if (or (zero? remaining) (null? list))
        (values (reverse! taken) list)
        (if (predicate (car list))
            (loop (1- remaining)
                  (cons (car list) taken)
                  (cdr list))
            (loop (1- remaining)
                  taken list)))))


;; Returns the cross product between l1 and l2.
;; each element is a cons cell.
(define (cross-product% l1 l2)
  (concatenate
   (map (lambda (a)
          (map (lambda (b) (cons a b))
               l2))
        l1)))

(define (cross-product . args)
  (if (null? args)
      '()
      (let ((last rest (car+cdr (reverse args))))
        (reduce-right cross-product% '()
                      (reverse (cons (map list last) rest ))))))

;; Given an arbitary tree, do a pre-order traversal, appending all strings.
;; non-strings allso allowed, converted to strings and also appended.
(define (string-flatten tree)
  (cond [(string? tree) tree]
        [(list? tree) (string-concatenate (map string-flatten tree))]
        [else (format #f "~a" tree)]))

(define (intersperse item list)
  (let loop ((flipflop #f)
             (rem list))
    (if (null? rem)
        '()
        (if flipflop
            (cons item (loop (not flipflop) rem))
            (cons (car rem) (loop (not flipflop) (cdr rem)))
            ))))

;; @example
;; (insert-ordered 5 (iota 10))
;; ⇒ (0 1 2 3 4 5 5 6 7 8 9)
;; @end example
(define* (insert-ordered item collection optional: (< <))
  (cond [(null? collection)
         (list item)]
        [(< item (car collection))
         (cons item collection)]
        [else
         (cons (car collection)
               (insert-ordered item (cdr collection) <))]))



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
;; srfi-9 gnu).
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

(define (downcase-symbol symb)
  (-> symb
      symbol->string
      string-downcase
      string->symbol))


;; @example
;; (group (iota 10) 2)
;; ⇒ ((0 1) (2 3) (4 5) (6 7) (8 9))
;; @end example
;; Requires that width|(length list)
(define (group list width)
  (unless (null? list)
    (let ((row rest (split-at list width)))
      (cons row (group rest width)))))

;; repeatedly apply @var{proc} to @var{base}
;; unitl @var{until} is satisfied.
;; (a → a), (a → bool), a → a
(define (iterate proc until base)
  (let loop ((o base))
    (if (until o)
        o
        (loop (proc o)))))

;; (a → values a), list ... → values a
(define (valued-map proc . lists)
  (apply values
   (apply append-map
          (lambda args
            (call-with-values (lambda () (apply proc args)) list))
          lists)))

(define (ass%-ref-all alist key =)
  (map cdr (filter (lambda (pair) (= key (car pair)))
                   alist)))

;; Equivalent to assoc-ref (and family), but works on association lists with
;; non-unique keys, returning all mathing records (instead of just the first).
;; @begin lisp
;; (assoc-ref-all '((a . 1) (b . 2) (a . 3)) 'a)
;; ⇒ (1 3)
;; @end
(define (assoc-ref-all alist key) (ass%-ref-all alist key equal?))
(define (assq-ref-all alist key)  (ass%-ref-all alist key eq?))
(define (assv-ref-all alist key)  (ass%-ref-all alist key eqv?))


(define (uniqx = lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        ((and (pair? lst)
              (= (car lst) (cadr lst)))
         (uniqx = (cons (car lst) (cddr lst))))
        (else (cons (car lst)
                    (uniqx = (cdr lst))))))

(define (uniq lst) (uniqx eq? lst))
(define (univ lst) (uniqx eqv? lst))
(define (unique lst) (uniqx equal? lst))



(define (vector-last v)
  (vector-ref v (1- (vector-length v))))

(define (->string any)
  (with-output-to-string (lambda () (display any))))



(define-syntax catch*
  (syntax-rules (pre-unwind)
    ((_ thunk ((pre-unwind key) handler))
     (with-throw-handler (quote key) thunk handler))
    ((_ thunk (key handler))
     (catch (quote key) thunk handler))

    ((_ thunk pair rest ...)
     (catch* (lambda () (catch* thunk pair))
             rest ...))))

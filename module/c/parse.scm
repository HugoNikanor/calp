(define-module (c parse)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 match)
  :export (parse-lexeme-tree))

;;; Rename this
(define (perms set)
  (concatenate
   (map (lambda (key)
          (map (lambda (o) (cons key o))
               (delete key set)))
        set)))

(define (symbol-concat pair)
  (cond [(null? (car pair)) (cdr pair)]
        [(null? (cdr pair)) (car pair)]
        [else (symbol-append (car pair) (cdr pair))]))

(define (parse-integer-suffix str)
  (define valid-sequences
    (delete 'dummy
            (lset-union eq? '(dummy)
                        (map symbol-concat (perms '(() U L)))
                        (map symbol-concat (perms '(() U LL))))))

  ;; => (LLU ULL LL LU UL L U)

  (aif (memv (string->symbol (string-upcase str))
          valid-sequences)
       (case (car it)
         [(LLU ULL) '(unsigned long-long)]
         [(LU UL) '(unsigned long)]
         [(LL) '(long-long)]
         [(L) '(long)]
         [(U) '(unsigned)])
       (error "Invalid integer suffix")))

(define (parse-lexeme-tree tree)
  (match tree
    ['() '()]

    ;; Number constants
    [('base-10 n) (string->number n 10)]
    [('base-8  n) (string->number n  8)]
    [('base-16 n) (string->number n 16)]

    [('integer n ('integer-suffix suffix))
     `(as-type
       ,(parse-integer-suffix suffix)
       ,(parse-lexeme-tree n))
     ]
    [('integer n)
     (parse-lexeme-tree n)]

    ;; Character literals, stored as raw integers
    ;; so mathematical operations keep working on them.
    [('char ('escaped-char ('base-8-char n)))
     (-> n (string->number 8) #; integer->char)]
    [('char ('escaped-char ('base-16-char n)))
     (-> n (string->number 16) #; integer->char)]
    [('char ('escaped-char c))
     (char->integer
      (case (string-ref c 0)
        ((#\a) #\alarm)
        ((#\b) #\backspace)
        ((#\e) #\esc)
        ((#\f) #\page)
        ((#\n) #\newline)
        ((#\r) #\return)
        ((#\t) #\tab)
        ((#\v) #\vtab)
        ((#\\) #\\)
        ((#\') #\')))]
    [('char c) (char->integer (string-ref c 0))]

    [('variable var) (string->symbol var)]
    [('operator op)  (string->symbol op)]
    [('prefix-operator op)
     (case (string->symbol op)
       ((*) 'dereference)
       ((&) 'pointer)
       ((++) 'pre-increment)
       ((--) 'pre-decrement)
       (else => identity))]
    [('postfix-operator op)
     (case (string->symbol op)
       [(++) 'post-increment]
       [(--) 'post-decrement]
       [else => identity])]

    ;; Parenthesis grouping
    [('group args)
     (parse-lexeme-tree args)]

    ;; Atomic item. Used by flatten-infix
    [('atom body)
     (parse-lexeme-tree body)]

    [('prefix op arg)
     `(,(parse-lexeme-tree op)
       ,(parse-lexeme-tree arg))]

    [('postfix arg op)
     `(,(parse-lexeme-tree op)
       ,(parse-lexeme-tree arg))]

    [('infix args ...)
     (resolve-order-of-operations
      (flatten-infix (cons 'infix args)))]

    [('funcall function ('group arguments))
     `(funcall ,(parse-lexeme-tree function)
               ,(parse-lexeme-tree arguments))]

    [bare (throw 'parse-error
                 'parse-lexeme-tree
                 "Naked literal in lex-tree. How did that get there?"
                 '()
                 bare)]))

;; https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B

(define order-of-operations
  (reverse
   (concatenate
    ;; This is only for binary operations
    `((-> ,(symbol #\.))
      (* / %)
      (+ -)
      (<< >>)
      (< <= > >=)
      (== !=)
      (&)
      (^)
      (,(symbol #\|))
      (&&)
      (,(symbol #\| #\|))
      (= += -= *= /= %= <<= >>= &= ^= ,(symbol #\| #\=))
      (,(symbol #\,))
      ))))

(define (mark-other form)
  (if (list? form) (cons '*other* form) form))

(define* (resolve-order-of-operations
          tree optional: (order order-of-operations))

  (if (null? order)
      (car tree)
      (match tree
        [('*other* body ...) body]
        [(form) (resolve-order-of-operations form order)]
        [(forms ...)
         (match (split-by forms (car order))
           [(group) (resolve-order-of-operations group (cdr order))]
           [groups
            (cons (car order)
                  (map (lambda (form) (resolve-order-of-operations
                                  form order-of-operations))
                       groups))])]
        [a a])))

;; Flatens a tree of infix triples. Stops when it should.
;; (parenthesis, function calls, ...)
(define (flatten-infix form)
  (match form
    [('infix left op ('infix right ...))
     (cons* (parse-lexeme-tree left)
            (parse-lexeme-tree op)
            (flatten-infix (cons 'infix right)))]

    [('infix left op right)
     (list (mark-other (parse-lexeme-tree left))
           (parse-lexeme-tree op)
           (mark-other (parse-lexeme-tree right)))]

    [other (error "Not an infix tree ~a" other)]))




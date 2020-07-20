(define-module (vulgar parse-cpp)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 popen)
  :use-module (ice-9 peg)
  :use-module (ice-9 match)
  ;; required by define-define-peg-pattern
  :use-module ((system base compile) :select (compile))
  :use-module ((rnrs io ports) :select (call-with-port))
  :use-module (ice-9 pretty-print) ; used by one error handler
  :use-module ((util io) :select (read-lines))
  :use-module (util graph)
  )




;;; Simple operators are those which can be combined with '='
(define simple-operators
  `(+ - * / & ,(symbol #\|) ^ << >> % < > =))

;; apparently part of C
(define wordy-binary-operators
  '(bitand and_eq and bitor or_eq or xor_eq xor))

(define symbol-binary-operators
  (append (map (lambda (x) (symbol-append x '=)) simple-operators)
          `(&& ,(symbol #\| #\|) != ,(symbol #\,)
               -> ,(symbol #\.))
          simple-operators))

(define binary-operators
  (append symbol-binary-operators
          wordy-binary-operators))


;;; Lexer

;; Like the regular define-peg-pattern. But evaluates the
;; pattern before treating it as a peg rule.
(define-macro (define-define-peg-pattern name capture expr)
  `(define-peg-pattern ,name ,capture
     ;; NOTE how does this work if we are in a different module?
     ;; It currently however isn't a problem since we don't export
     ;; this macro.
     ,(eval expr (current-module))))

(define-peg-pattern base-8-digit body
  (range #\0 #\7))

(define-peg-pattern base-10-digit body
  (range #\0 #\9))

(define-peg-pattern base-16-digit body
  (or (range #\0 #\9)
      (range #\A #\F)
      (range #\a #\f)))

;; https://en.cppreference.com/w/cpp/language/integer_literal
(define-peg-pattern base-10 all (+ base-10-digit))
(define-peg-pattern base-8 all (and (ignore "0") (+ base-8-digit)))
(define-peg-pattern base-16 all (and (ignore (and "0" (or "x" "X")))
                                     (+ base-16-digit)))

;; accept anything now, ensure correctnes later
(define-peg-pattern integer-suffix all
  (* (or "u" "U" "l" "L")))

(define-peg-pattern integer all
  (and (or base-8 base-16 base-10) (? integer-suffix)))

(define-peg-pattern number body
  (or integer))

(define-peg-pattern group all
  (and (ignore "(") expr (ignore ")")))

(define-peg-pattern base-8-char all
  (and base-8-digit
       (? base-8-digit)
       (? base-8-digit)))

(define-peg-pattern base-16-char all
  (and (ignore "x") base-16-digit (? base-16-digit)))

(define-peg-pattern escaped-char all
  (and (ignore "\\") (or base-16-char
                         base-8-char
                         peg-any)))

(define-peg-pattern char all
  (and (ignore "'") (or escaped-char peg-any) (ignore "'")))

(define-define-peg-pattern operator all
  `(or ,@(map symbol->string symbol-binary-operators)
       ,@(map (lambda (op) `(and ,(symbol->string op) ws))
              wordy-binary-operators)))

;; whitespace
(define-peg-pattern ws none
  (or " " "	" "\n"))

;; space (for when whitespace is optional)
(define-peg-pattern sp none (* ws))

(define-peg-pattern safe-letter body
  (or "_"
      (range #\A #\Z)
      (range #\a #\z)))

(define-peg-pattern variable all
  (and safe-letter
       (* (or safe-letter
              base-10-digit))))

;; No further subparsing can be done.
;; NOTE that strings are generally also in this category.
(define-peg-pattern atom all
  (or base-8 base-10 base-16 number char variable))

(define-peg-pattern prefix-operator all
  (or "!" "~" "*" "&" "++" "--" "+" "-"))

;;; Note that stacked pre or postfix operators without parenthesis
;;; dosen't work. So `*&C' is invalid, while `*(&C)' is valid.

(define-peg-pattern prefix all
  (and prefix-operator sp (or variable group funcall #; postfix
                              )))

(define-peg-pattern postfix-operator all
  (or "++" "--"))

(define-peg-pattern postfix all
  ;; literals can't be in-place incremented and decremented
  ;; Make sure we don't match postfix-operator here, since
  ;; that also gives us an infinite loop.
  (and (or prefix funcall group variable) sp postfix-operator))

(define-peg-pattern infix all
  ;; first case is "same" as expr, but in different order to prevent
  ;; infinite self reference. Pre and postfix not here, solved by having
  ;; them before infix in expr
  (and (or funcall postfix prefix group char number variable)
       sp operator sp expr))

(define-peg-pattern funcall all
  (and variable sp group))

;;; main parser
(define-peg-pattern expr body
  (+ (and sp (or infix postfix prefix funcall group char number variable)
          sp)))


(define (lex string)
  (peg:tree (match-pattern expr string)))


;;; Parser

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
   (apply append
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
                  (map (lambda (form) (resolve-order-of-operations form order-of-operations))
                       groups))])]
        [a a])))

(define (mark-other form)
  (if (list? form) (cons '*other* form) form))

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
           (mark-other (parse-lexeme-tree right)))
     #; (map parse-lexeme-tree (list left op right))]
    [other (error "Not an infix tree ~a" other)]))


(define (do-funcall function arguments)
  (if (list? arguments)
      (apply function arguments)
      (function arguments)))

(define-public (replace-symbols tree dict)
  (if (not (list? tree))
      (or (assoc-ref dict tree) tree)
      (map (lambda (node) (replace-symbols node dict))
           tree)))




;; input "#define F(x, y) x + y"
;; 1 full define | F(x, y)
;; 2 macro name  | F
;; 3 macro args  | (x, y)
;; 4 macro body  | x + y
(define define-re (make-regexp "^#define ((\\w+)(\\([^)]*\\))?) (.*)"))

(define (tokenize-define-line header-line)
  (aif (regexp-exec define-re header-line)
    (cons (match:substring it 1)
          (match:substring it 4))
    (error "Line dosen't match" header-line)))

(define (tokenize-header-file header-file)
  (map tokenize-define-line
       (call-with-port
        (open-input-pipe
         (string-append "cpp -dM " header-file))
        read-lines)))

(define symb-map
  `((,(symbol #\|) . logior)
    (funcall . do-funcall)
    (&& . and)
    (& . logand)
    (== . =)
    (!= . (negate =))
    ))

(define (atom? x)
  ;; NOT symbol
  (or (number? x)
      (char? x)
      (string? x)))

;; built in symbols. Should never be marked as dependencies
(define (primitive? x)
  (memv x (cons 'funcall binary-operators)))

;; -> (list (dependencies . symbol . value)
(define (parse-cpp-define pair)
  (define f (compose parse-lexeme-tree lex))
  (define left (f (car pair)))
  (define proc-args
    (match (and (pair? left)
                (eq? 'funcall (car left))
                (caddr left))
      [#f '()]
      [(_ args ...) args]
      [arg (list arg)]))

  (define right (f (cdr pair)))
  (define alt-right (replace-symbols right symb-map))
  (define dependencies
    (lset-difference
     eq?
     (remove primitive?
             (remove atom?
                     (flatten (if (list? right)
                                  right (list right)))))
     proc-args))

  (cons
   dependencies
   (match left
     [('funcall name ('#{,}# args ...))
      (cons name `(lambda ,args ,alt-right))]

     [('funcall name arg)
      (cons name `(lambda (,arg) ,alt-right))]

     [name (cons name alt-right)])))


(define source-form (make-object-property))

(define (parse-cpp-file lines)
  (for (i line) in (enumerate lines)
       (catch #t
         (lambda ()
           (let ((def (parse-cpp-define line)))
             (set! (source-form def)
               (format #f "#define ~a ~a" (car line) (cdr line)))
             def))
         (lambda (err caller fmt args . _)
           (format #t "~a ~?~%" i fmt args) #f))))

(define (private-c-symbol? string)
  (char=? #\_ (string-ref string 0)))

(define-macro (include# header-file)
  (define lines (remove (compose private-c-symbol? car)
                        (tokenize-header-file header-file)))

  (define forms (parse-cpp-file lines))

  (define graph*
    (fold (lambda (node graph)
            (set! (source-form (cdr node))
              (source-form node))
            (add-node graph (cdr node) (car node)))
          (make-graph car)
          (filter identity forms)))

  ;; Hack for termios since this symbol isn't defined.
  ;; (including in the above removed private c symbols)
  (define graph (add-node graph* (cons '_POSIX_VDISABLE #f) '()))

  `(begin
     ,@(for (key . value) in (resolve-dependency-graph graph)
            `(define ,key ,value)))))


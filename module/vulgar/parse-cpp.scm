(define-module (vulgar parse-cpp)
  :use-module (util)
  :use-module (ice-9 popen)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 peg)
  :use-module (ice-9 match)
  )


(define (read-lines port)
  (with-input-from-port port
    (lambda ()
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '() (cons line (loop (read-line))))))))

(define (parse-header header-file)
  (map (lambda (line)
         (let* ((symbol (string-index line #\space))
                (value (string-index line #\space (1+ symbol))))
           (cons (substring line (1+ symbol) value)
                 (substring line (1+ value)))))
   (read-lines (open-input-pipe (string-append "cpp -dM " header-file))))

  #;
  (let* (((_ key . values) (string-split line #\space)))
    (if (char=? #\_ (string-ref key 0))
        (loop (read-line))
        (cons (cons key (string-join values " " 'infix))
              (loop (read-line)))))



  )



;;; Lexer


(define-peg-pattern base-8-digit body
  (range #\0 #\7))

(define-peg-pattern base-10-digit body
  (range #\0 #\9))

(define-peg-pattern base-16-digit body
  (or (range #\0 #\9)
      (range #\A #\F)
      (range #\a #\f)))

(define-peg-pattern base-10 all (+ base-10-digit))
(define-peg-pattern base-8 all (and "0" (+ base-8-digit)))
(define-peg-pattern base-16 all (and "0x" (+ base-16-digit)))

(define-peg-pattern number body
  (or base-8 base-16 base-10))

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

;; (define-peg-pattern string body
;;   (and "\"" (+ (or "\\\"" peg-any)) "\""))

;;; Simple operators are those which can be combined with '='
(define-peg-pattern simple-operator body
  (or "+" "-" "*" "/" "&" "|" "^" "<<" ">>" "%"
      "<" ">" "="))

(define-peg-pattern operator all
  (or (and simple-operator "=")
      "&&" "||"
      simple-operator
      "!=" ","
      "and" "bitand" "and_eq"
      "or" "bitor" "or_eq"
      "xor" "xor_eq"
      ;; "->" "." ; special cases since can only be used with variables
      ;; Todo Ternaries
      ))


;; whitespace
(define-peg-pattern ws none
  (or " " "	" "\n"))

;; space (for when whitespace is optional)
(define-peg-pattern sp none
  (* ws))

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

;;; ++ and -- both pre and postfix

(define-peg-pattern prefix-operator all
  (or "!" "~" "*" "&" "++" "--" "+" "-"))

(define-peg-pattern prefix all
  (and prefix-operator sp expr))

(define-peg-pattern postfix-operator all
  (or "++" "--"))

(define-peg-pattern postfix all
  ;; literals can't be in-place incremented and decremented
  ;; Make sure we don't match postfix-operator here, since
  ;; that also gives us an infinite loop.
  (and (or prefix infix funcall group variable) sp postfix-operator))

;; 5 + 3 * 9
;; (5 + 3) * 9
;; 5 + (3 * 9)
(define-peg-pattern infix all
  ;; first case is "same" as expr, but in different order to prevent
  ;; infinite self reference.
  (and (or funcall group char prefix #; postfix number variable
           ) sp operator sp expr))

(define-peg-pattern funcall all
  (and variable sp group))

;;; main parser
(define-peg-pattern expr body
  (+ (and sp (or prefix #; postfix infix funcall group char number variable
                 ) sp)))


(define (lex string)
  (peg:tree (match-pattern expr string)))


;;; Parser


(define (parse-lexeme-tree tree)
  (match tree
    ['() '()]

    ;; Number constants
    [('base-10 n) (string->number n 10)]
    [('base-8  n) (string->number n  8)]
    [('base-16 n) (string->number n 16)]

    ;; Character literals
    [('char ('escaped-char ('base-8-char n)))
     (-> n (string->number 8) integer->char)]
    [('char ('escaped-char ('base-16-char n)))
     (-> n (string->number 16) integer->char)]
    [('char ('escaped-char c))
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
       ((#\') #\'))]
    [('char c) (string-ref c 0)]

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

    [bare (error "Naked literal in lex-tree. How did that get there?"
                 bare)]))

;;; TODO
;; (f "*C++")
;; $427 = (dereference (post-increment C))

(define (group-by list item)
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

;; https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B

(define order-of-operations
  (reverse
   (apply append
          ;; This is only for binary operations
    `((* / %)
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


;; (f "2 + (2*2)")
;; <unnamed port>:5967:23: In procedure resolve-order-of-operations:
;; In procedure car: Wrong type argument in position 1 (expecting pair): ()

;; Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.

(define* (resolve-order-of-operations
          tree optional: (order order-of-operations))

  (cond [(null? order) (car tree)]
        [(not (list? tree)) tree]
        [(= 1 (length tree)) (resolve-order-of-operations
                              (car tree) order)]
        [else
         (let ((groups (group-by tree (car order))))
           (cond [(= 1 (length groups))
                  (resolve-order-of-operations
                   (car groups) (cdr order))]
                 [else
                  (cons (car order)
                        (append
                         (map (lambda (g) (resolve-order-of-operations
                                      g (cdr order)))
                              groups)))]))]))

;; Flatens a tree of infix triples. Stops when it should.
(define (flatten-infix form)
  (match form
    [('infix left op ('infix right ...))
     (cons* (parse-lexeme-tree left)
            (parse-lexeme-tree op)
            (flatten-infix (cons 'infix right)))]
    [('infix left op right)
     (map parse-lexeme-tree (list left op right))]
    [other (parse-lexeme-tree other)]))


;; scheme@(vulgar parse-cpp)> (match-pattern expr "a xorb")
;; $10 = #<peg start: 0 end: 6 string: a xorb tree: (infix (variable a) (operator xor) (variable b))>


(define (do-funcall function arguments)
  (if (list? arguments)
      (apply function arguments)
      (function arguments)))

(define-public (replace-symbols tree dict)
  (if (not (list? tree))
      (or (assoc-ref dict tree) tree)
      (map (lambda (node) (replace-symbols node dict))
           tree)))

(define f (compose parse-lexeme-tree lex))

;;; Right, when left simple binding
;; direct constant (int|char)
;; (op forms ...)
;; (do-funcall forms ...)
;; direct variable

(define (parse-cpp-define pair)
  (define left (f (car pair)))
  (define right (replace-symbols
                 (f (cdr pair))
                 `((,(symbol #\|) . logior)
                   (funcall . do-funcall)
                   (&& . and)
                   (& . logand)
                   (== . =)
                   (!= . (negate =))
                   )))

  (match left
    [('funcall name ('#{,}# args ...))
     `(define (,name ,@args)
        ,right)]

    [('funcall name arg)
     `(define (,name ,arg)
        ,right)]

    [name `(define ,name ,right)]))

;;; TODO order of these, to resolve dependencies
(define (parse-cpp-file file)
  ;; (map parse-cpp-define (parse-header file))
  (map (lambda (i line) (catch #t (lambda () (parse-cpp-define line))
                     (lambda (err caller fmt args . _) (format #t "~a ~?~%" i fmt args) #f)))
       (iota (length (parse-header file)) 1)
       (parse-header file)))

;; (parse-cpp-file "/usr/include/termios.h")

(begin
  (define file (open-output-file "/tmp/termios.scm"))
  (define lines (parse-cpp-file "/usr/include/termios.h"))

  (for-each (lambda (line) (format file "~y" line))
            lines)
  (close-port file))

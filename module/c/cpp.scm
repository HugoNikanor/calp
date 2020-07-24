(define-module (c cpp)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 popen)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module ((rnrs io ports) :select (call-with-port))
  :use-module (ice-9 pretty-print) ; used by one error handler
  :use-module ((util io) :select (read-lines))
  :use-module (util graph)
  :use-module (c lex)
  :use-module (c parse)
  :use-module (c operators)
  )


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


(define-public (do-funcall function arguments)
  (if (list? arguments)
      (apply function arguments)
      (function arguments)))

(define symb-map
  `((,(symbol #\|) . logior)
    (funcall . (@ (c cpp) do-funcall))
    (&& . and)
    (& . logand)
    (== . =)
    (!= . (negate =))
    ))

(define-public (replace-symbols tree dict)
  (if (not (list? tree))
      (or (assoc-ref dict tree) tree)
      (map (lambda (node) (replace-symbols node dict))
           tree)))

(define (atom? x)
  ;; NOT symbol
  (or (number? x)
      (char? x)
      (string? x)))

;; built in symbols. Should never be marked as dependencies
(define (primitive? x)
  (memv x (cons 'funcall binary-operators)))



;; (symbol . value) -> (list (dependencies . symbol . value)
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


(define (parse-cpp-file lines)
  (map (lambda (line)
         (catch #t
           (lambda () (parse-cpp-define line))
           (lambda (err caller fmt args . _)
             (format #t "~a ~?~%" fmt args)
             #f)))
       lines))

(define (private-c-symbol? string)
  (char=? #\_ (string-ref string 0)))

(define (tokenize-header-file header-file)
  (map tokenize-define-line
       (call-with-port
        (open-input-pipe
         (string-append "cpp -dM " header-file))
        read-lines)))

(define-macro (include# header-file . args)

  (define define-form (if (null? args) 'define (car args)))

  (define lines (remove (compose private-c-symbol? car)
                        (tokenize-header-file header-file)))

  (define forms (parse-cpp-file lines))

  (define graph*
    (fold (lambda (node graph)
            (add-node graph (cdr node) (car node)))
          (make-graph car)
          (filter identity forms)))

  ;; Hack for termios since this symbol isn't defined.
  ;; (including in the above removed private c symbols)
  (define graph (add-node graph* (cons '_POSIX_VDISABLE #f) '()))

  `(begin
     ,@(map (lambda (pair) `(,define-form ,(car pair) ,(cdr pair)))
            (resolve-dependency-graph graph))))


(export include#)

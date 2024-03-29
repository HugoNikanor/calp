(define-module (c cpp)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 popen)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module ((rnrs io ports) :select (call-with-port))
  :use-module (ice-9 format)
  :use-module ((hnh util io) :select (read-lines))
  :use-module (hnh util graph)
  :use-module (c lex)
  :use-module (c parse)
  :use-module (c operators)
  :export (do-funcall replace-symbols include#)
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
    (scm-error 'c-parse-error
               "tokenize-define-line"
               "Line dosen't match: ~s"
               (list header-line) #f)))


(define (do-funcall function arguments)
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

(define (replace-symbols tree dict)
  (if (not (list? tree))
      (or (assoc-ref dict tree) tree)
      (map (lambda (node) (replace-symbols node dict))
           tree)))

;; Direct values. Lisp also has quoted symbols in this group.
(define (immediate? x)
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
             (remove immediate?
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
           (lambda (err caller fmt args data)
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

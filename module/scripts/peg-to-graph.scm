(define-module (scripts peg-to-graph)
  :use-module ((graphviz) :prefix #{gv:}#)
  :use-module ((hnh module-introspection) :select (unique-symbols))
  :use-module ((hnh module-introspection static-util) :select (get-forms))
  :use-module (srfi srfi-1)
  :use-module (ice-9 match)
  :export (main))

(define %summary "Output peg-pattern relations as a graphviz graph.")
(define %include-in-list #t)

(define peg-primitives
  '(and or * + ? followed-by not-followed-by peg-any range
        ignore capture peg))

(define (handle-peg-form! graph form)
  (match form
    (`(define-peg-pattern ,name ,capture ,body)
     (let ((node (gv:node graph (format #f "~a" name))))
       (gv:setv node "style"
                (case capture
                  ((all) "solid")
                  ((body) "dashed")
                  ((none) "dotted"))))
     (for-each (lambda (symbol)
                 (gv:edge graph
                          (format #f "~a" name)
                          (format #f "~a" symbol)))
               (remove (lambda (x) (memv x peg-primitives))
                       (unique-symbols (list body)))))))

(define (main . args)
  (when (< 1 (length args))
    (format #t "Usage: guild peg-to-graph <filename>~%")
    (exit 1))

  (let ((graph (gv:digraph "G")))
    (let ((input-file (car args)))
      (for-each (lambda (form) handle-peg-form! graph form)
                (filter (lambda (x)
                          (and (list? x)
                               (not (null? x))
                               (eq? 'define-peg-pattern (car x))))
                        (call-with-input-file input-file get-forms))))

    (gv:layout graph "dot")
    (gv:render graph "pdf" "lex2.pdf")

    (display "done\n")))

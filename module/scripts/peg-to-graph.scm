(define-module (scripts peg-to-graph)
  :use-module ((graphviz) :prefix #{gv:}#)
  :use-module ((hnh module-introspection) :select (unique-symbols))
  :use-module ((hnh module-introspection static-util) :select (get-forms))
  :use-module (srfi srfi-1)
  :use-module (ice-9 match)
  :use-module (hnh util options)
  :use-module (ice-9 getopt-long)
  :export (main))

(define option-spec
  `((engine (value #t)
            (description "Graphviz rendering engine to use. Defaults to DOT"))
    (output (single-char #\o)
            (value #t)
            (description "Name of output pdf"))))

(define %summary "Output peg-pattern relations as a graphviz graph.")
(define %synopsis "peg-to-graph [options] <filename>")
(define %help (format-arg-help option-spec))

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
  (define options (getopt-long (cons "peg-to-graph" argrs)
                               (getopt-opt option-spec)))
  (define engine (option-ref options 'engine "dot"))
  (define output-file (option-ref options 'output "lex2.pdf"))
  (define input-file (let ((filenames (option-ref options '() '())))
                       (when (null? filenames)
                         (format #t "Usage: ~a~%" %summary)
                         (exit 1))
                       (car filenames)))


  (let ((graph (gv:digraph "G")))
    (for-each (lambda (form) handle-peg-form! graph form)
              (filter (lambda (x)
                        (and (list? x)
                             (not (null? x))
                             (eq? 'define-peg-pattern (car x))))
                      (call-with-input-file input-file get-forms)))

    (gv:layout graph engine)
    (gv:render graph "pdf" output-file)))

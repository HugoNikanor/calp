#!/usr/bin/env bash
GUILE=${GUILE:-guile}
set -x
exec $GUILE -e main -s "$0" "$@"
!#

(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/use2dot"))


(use-modules ((graphviz) :prefix #{gv:}#)
             ((module-introspection) :select (unique-symbols))
             ((static-util) :select (get-forms))
             (srfi srfi-1)
             (ice-9 match))

(define peg-primitives
  '(and or * + ? followed-by not-followed-by peg-any range
        ignore capture peg))

(define graph (gv:digraph "G"))

(define (handle-peg-form form)
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

(define (main args)
  (when (< 2 (length args))
    (format #t "Usage: ~a <filename>~%" (car args))
    (exit 1))

  (let ((input-file (cadr args)))
    (for-each handle-peg-form
     (filter (lambda (x)
               (and (list? x)
                    (not (null? x))
                    (eq? 'define-peg-pattern (car x))))
             (call-with-input-file input-file get-forms))))

  (gv:layout graph "dot")
  (gv:render graph "pdf" "lex2.pdf")

  (display "done\n"))



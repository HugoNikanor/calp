(define-module (test module-introspection static-util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh module-introspection static-util))

(test-equal "All forms"
    '((define (f x)
        (* x 2))
      (define-module (a)
        :use-module (srfi srfi-1)
        :export (f)))
  (call-with-input-file "tests/test-module-tree/a.scm" get-forms))


'((hnh module-introspection static-util))

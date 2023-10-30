(define-module (test module-introspection)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  ;; :use-module ((hnh util) :select (->))
  :use-module (hnh module-introspection))

(test-equal "Unique Symbols"
  '(* +)
  (unique-symbols '(+ (* 2 (+ 3 4))
                      5)))

(test-equal
    '(my module)
    (find-module-declaration
     '((define-module (my module)
         :export (nothing))
       (define nothing '()))))

(test-equal "All forms"
    '((define (f x)
        (* x 2))
      (define-module (a)
        :use-module (srfi srfi-1)
        :export (f)))
  (call-with-input-file "tests/test-module-tree/a.scm" get-forms))


'((hnh module-introspection))

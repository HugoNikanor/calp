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

'((hnh module-introspection))

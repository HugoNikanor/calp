(define-module (test test-match-path)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (hnh test util))



(define (runner-full-path! runner path)
  ;; NOTE
  ;; This uses internal implementation details. Technically, the
  ;; "approved" way would be through `test-apply` and a callback
  ;; procedure ran inside a a dynamicly created context through
  ;; `test-group` and `test-*` (turning this procedure into a macro
  ;; along the way).
  (let ((name stack (car+cdr (reverse path))))
    ((@@ (srfi srfi-64) test-runner-group-stack!)
     runner stack)
    ((@@ (srfi srfi-64) test-runner-test-name!)
     runner name)))

(let ((runner (test-runner-null)))
  (runner-full-path! runner '("a" "b" "c"))
  (test-assert "Exact match" ((test-match-path "a" "b" "c") runner))
  (test-assert "contains \"b\"" ((test-match-path ... "b" ...) runner))
  (test-assert "contains \"a\" (even if at start)" ((test-match-path ... "a" ...) runner))
  (test-assert "contains \"c\" (even if at end)" ((test-match-path ... "c" ...) runner))
  (test-assert "Ends with \"c\"" ((test-match-path ... "c") runner))
  (test-assert "Does NOT end with \"b\"" (not ((test-match-path ... "b") runner)))
  (test-assert "Does not contain \"d\"" (not ((test-match-path ... "d" ...) runner))))



'((hnh test util))

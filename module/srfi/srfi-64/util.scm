(define-module (srfi srfi-64 util)
  :use-module (ice-9 curried-definitions)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-64)
  :export (test-match-group))

;; Specifier for name of group
(define ((test-match-group name . names) runner)
  (every string=?
         (reverse (cons name names))
         (test-runner-group-stack runner)))

(define-module (test geo)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (vcomponent geo))

(test-assert (geo? (geo x: 10 y: 20)))
(test-error 'wrong-type-arg
  (geo))

'((vcomponent geo))

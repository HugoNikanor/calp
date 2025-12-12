(define-module (test text-big-numbers)
  :use-module (srfi srfi-64)
  :use-module (text big-numbers))

(test-assert (power-of-10? #e1e100))
(test-assert (power-of-10? 1e100))
;; (test-assert (not (power-of-10? (+ #e1e100 1))))
;; (test-assert (not (power-of-10? (+ 1e100   1))))
;; (test-assert (not (power-of-10? (- #e1e100 1))))
;; (test-assert (not (power-of-10? (- 1e100   1))))

(test-equal "₉₆₈₀₂₃₇₄₅₁" (subscript 9680237451))
(test-equal "⁹⁶⁸⁰²³⁷⁴⁵¹" (supscript 9680237451))

(test-equal "10¹⁰⁰"
  (number->exponential-form #e1e100))

(test-error 'wrong-type-arg
  (number->exponential-form 20))

'((text big-numbers))

;;; Commentary:
;; Tests my custom let*.
;;; Code:

(define-module (test let)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (let*)))

(test-assert (let* ((a #t)) a))

(test-assert (let* (((a . b) (cons #t #f))) a))

(test-assert (let* (((a . b) (cons* #f #t))) b))

(test-assert
  (let* ((a b c (values #f #t #f))) b))

(test-assert
  (let* (((a b c) (list #f #t #f))) b))

(test-assert (let* (((a) '(#t))) a))

(test-equal '(2) (let* (((a . b) '(1 2))) b))

(test-equal
  '(3 4)
  (let* (((a b . c) '(1 2 3 4))) c))

(test-equal 10 (let* (x) (set! x 10) x))

(test-equal
  30
  (let* (x y) (set! x 10) (set! y 20) (+ x y)))

(test-assert (let* (x) (not x)))

(test-equal
  6
  (let* ((x 1) y z)
    (set! y 2)
    (set! z 3)
    (+ x y z)))



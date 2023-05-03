(define-module (test hnh-util-lens)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util lens))


(define first (ref 0))

(test-equal '((1)) (first '(((1)))))
(test-equal '((2)) (set '(((1))) (compose-lenses first first) 2))
(test-equal '(((2))) (set '(((1))) (compose-lenses first first first) 2))


;; (list-change (iota 10) 5 'Hello)
;; => (0 1 2 3 4 Hello 6 7 8 9)

(test-equal '(1 (10) 3) (set '(1 (2) 3) (compose-lenses (ref 1) (ref 0)) 10))
(test-equal '(1 (10) 3) (set '(1 (2) 3) (ref 1) (ref 0) 10))

;; (set (list (iota 10)) first first 11)

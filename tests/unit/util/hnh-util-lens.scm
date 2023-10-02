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

(define cadr* (compose-lenses cdr* car*))

(test-group "Primitive lenses get and set"
  (define lst '(1 2 3 4 5))
  (test-equal 1 (car* lst))
  (test-equal '(2 3 4 5) (cdr* lst))

  (test-equal '(10 2 3 4 5)
    (car* lst 10)))

(test-group "Primitive lens composition"
  (define lst '(1 2 3 4 5))
  (test-equal 2 (cadr* lst))
  (test-equal '(1 10 3 4 5) (cadr* lst 10)))

(test-group "Modify"
  (define lst '(1 2 3 4 5))
  (test-equal '(10 2 3 4 5) (modify lst car* * 10))
  (test-equal '(1 20 3 4 5) (modify lst cadr* * 10))
  )

(test-group "Modify*"
  (define lst '(1 2 3 4 5))
  (test-equal '(1 2 4 4 5) (modify* lst cdr* cdr* car* 1+)))

;; modify
;; modify*
;; set
;; get

;; identity-lens
;; compose-lenses
;; lens-compose

;; ref car* cdr*

;; each

'((hnh util lens))

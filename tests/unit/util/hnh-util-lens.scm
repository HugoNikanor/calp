(define-module (test hnh-util-lens)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (enumerate))
  :use-module (hnh util lens))


(define first (ref 0))

(test-equal '((1)) (get '(((1))) first))

(test-equal '((2)) (set '(((1))) (compose-lens first first) 2))
(test-equal '(((2))) (set '(((1))) (compose-lens first first first) 2))


;; (list-change (iota 10) 5 'Hello)
;; => (0 1 2 3 4 Hello 6 7 8 9)

(test-equal '(1 (10) 3) (set '(1 (2) 3)
                             (compose-lens (ref 1) (ref 0))
                             10))
(test-equal '(1 (10) 3) (set '(1 (2) 3)
                             (compose-lens (ref 1) (ref 0))
                             10))

;; (set (list (iota 10)) first first 11)

(define cadr* (compose-lens cdr* car*))

(test-group "Primitive lenses get and set"
  (define lst '(1 2 3 4 5))
  (test-equal 1 (get lst car*))
  (test-equal '(2 3 4 5) (get lst cdr*))

  (test-equal '(10 2 3 4 5)
    (set lst car* 10)))

(test-group "Primitive lens composition"
  (define lst '(1 2 3 4 5))
  (test-equal 2 (get lst cadr*))
  (test-equal '(1 10 3 4 5) (set lst cadr* 10)))

(test-group "Modify"
  (define lst '(1 2 3 4 5))
  (test-equal '(10 2 3 4 5) (modify lst car*  (lambda (x) (* x 10))))
  (test-equal '(1 20 3 4 5) (modify lst cadr* (lambda (x) (* x 10))))
  )

(test-group "Modify*"
  (define lst '(1 2 3 4 5))
  (test-equal '(1 2 4 4 5)
    (modify lst (lens-compose cdr* cdr* car*) 1+)))

(test-equal
    "!e!l!,!W!r!d"
  (list->string
   (map cdr
        (set (enumerate (string->list "Hello, World"))
             (compose-lens
              (focus-matching (compose even? car))
              cdr*)
             #\!))))


(test-group "Identity lens"
  (test-equal 'anything
    (get 'anything identity-lens))
  (test-equal 'else
    (set 'anything identity-lens 'else))

  (test-equal '(1 x 3)
    (set '(1 2 3) (lens-compose cdr* identity-lens car*)
         'x)))


'((hnh util lens))

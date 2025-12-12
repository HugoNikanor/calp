(define-module (test hnh-util-type)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util type))

;;; NOTE current-procedure-name is more or less unstentable
;;; Since it is so dependant on how things are evaluated.

(test-assert "Basic positive test"
  (typecheck 1 integer?))
(test-error "Basic negative test"
  'wrong-type-arg (typecheck 1 string?))

(test-assert "Basic compound test"
  (typecheck (cons 1 "Hello") (pair-of integer? string?)))
(test-assert "Basic list-of test"
  (typecheck (iota 10) (list-of integer?)))
(test-error "Negative list-of test"
  'wrong-type-arg (typecheck (append (iota 10) '("10"))
                             (list-of integer?)))

(test-assert "List of pairs"
  (typecheck '((1 . "one") (2 . "two"))
             (list-of (pair-of integer? string?))))

(test-error "Negative test for list of pairs"
  'wrong-type-arg
  (typecheck '((1 . "one") (2 . "two") ("three" . 3))
             (list-of (pair-of integer? string?))))

(test-assert "Basic type intersection test"
  (typecheck 1 (and integer? positive?)))
(test-assert "Basic type union test (alt 1)"
  (typecheck 1 (or integer? string?)))
(test-assert "Basic type unino test (alt 2)"
  (typecheck "1" (or integer? string?)))
(test-assert "Basic intersection with negative part"
  (typecheck 1 (and integer? (not zero?))))
(test-error "Negative test for intersection with negative part"
  'wrong-type-arg
  (typecheck 0 (and integer? (not zero?))))

'((hnh util type))

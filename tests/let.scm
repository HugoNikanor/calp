(use-modules (util))

(test-begin "let")

(test-assert (let* ((a #t)) a))
(test-assert (let* (((a . b) '(#t . #f))) a))
(test-assert (let* (((a . b) (cons* #f #t))) b))
(test-assert (let* ((a b c (values #f #t #f))) b))
(test-assert (let* (((a b c) (list #f #t #f))) b))
(test-assert (let* (((a) '(#t))) a))
(test-equal '(2) (let* (((a . b) '(1 2))) b))
(test-equal '(3 4) (let* (((a b . c) '(1 2 3 4))) c))

(test-end "let")

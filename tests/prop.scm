(use-modules (vcomponent))

(define v (make-vcomponent
           (string-append (getenv "TESTPATH")
                          "/prop.ics")))

(test-equal '("1") (prop (attr* v 'KEY) 'A))
(test-equal '("2") (prop (attr* v 'KEY) 'B))
(test-equal #f (prop (attr* v 'KEY) 'C))

(test-equal '(A B) (map car (properties (attr* v 'KEY))))

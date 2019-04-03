(use-modules (vcalendar))

(define v (make-vcomponent
           (string-append (getenv "TESTPATH")
                          "/prop.ics")))

(test-begin "Proporty test")
(test-equal (prop v 'KEY 'A) '(("1")))
(test-equal (prop v 'KEY 'B) '(("2")))
(test-equal (prop v 'KEY 'C) '())

(test-equal (properties v 'KEY) '(A B))
(test-equal (properties v "KEY") '(A B))

(test-end "Proporty test")

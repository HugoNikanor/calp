;;; Commentary:
;; Test that vcomponent parsing works at all.
;;; Code:

(((vcomponent base) prop)
 ((vcomponent formats ical parse) parse-calendar))

(define ev (call-with-input-string
               "BEGIN:DUMMY
X-KEY:value
END:DUMMY"
             parse-calendar))

(test-assert (eq? #f (prop ev 'MISSING)))
(test-assert (prop ev 'X-KEY))
(test-equal "value" (prop ev 'X-KEY))

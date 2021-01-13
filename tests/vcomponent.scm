;;; Commentary:
;; Test that vcomponent parsing works at all.
;;; Code:

(((vcomponent base) prop)
 ((vcomponent) parse-calendar))

(define ev (call-with-input-string
               "BEGIN:DUMMY
KEY:value
END:DUMMY"
             parse-calendar))

(test-assert (eq? #f (prop ev 'MISSING)))
(test-assert (prop ev 'KEY))
(test-equal "value" (prop ev 'KEY))

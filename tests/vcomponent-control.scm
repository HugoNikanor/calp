;;; Commentary:
;; Tests that with-replaced-properties work.
;;; Code:

(((vcomponent util control) with-replaced-properties)
 ((vcomponent formats ical parse) parse-calendar)
 ((vcomponent base) prop))



(define ev (call-with-input-string
               "BEGIN:DUMMY
X-KEY:value
END:DUMMY"
             parse-calendar))

;; Test that temoraries are set and restored
(test-equal "value" (prop ev 'X-KEY))
(with-replaced-properties (ev (X-KEY "other"))
                          (test-equal "other" (prop ev 'X-KEY)))
(test-equal "value" (prop ev 'X-KEY))

;; Test that they are restored on non-local exit
(catch #t
  (lambda ()
   (with-replaced-properties (ev (X-KEY "other"))
                             (throw 'any)))
  (lambda _
    (test-equal "value" (prop ev 'X-KEY))))

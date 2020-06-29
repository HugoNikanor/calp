(((vcomponent control) with-replaced-properties)
 ((vcomponent) parse-calendar)
 ((vcomponent base) prop))



(define ev (call-with-input-string
               "BEGIN:DUMMY
KEY:value
END:DUMMY"
             parse-calendar))

;; Test that temoraries are set and restored
(test-equal "value" (prop ev 'KEY))
(with-replaced-properties (ev (KEY "other"))
                          (test-equal "other" (prop ev 'KEY)))
(test-equal "value" (prop ev 'KEY))

;; Test that they are restored on non-local exit
(catch #t
  (lambda ()
   (with-replaced-properties (ev (KEY "other"))
                             (throw 'any)))
  (lambda _
    (test-equal "value" (prop ev 'KEY))))

(((vcomponent control) with-replaced-attrs)
 ((vcomponent) parse-calendar)
 ((vcomponent base) attr))



(define ev (call-with-input-string
               "BEGIN:DUMMY
KEY:value
END:DUMMY"
             parse-calendar))

;; Test that temoraries are set and restored
(test-equal "value" (attr ev 'KEY))
(with-replaced-attrs (ev (KEY "other"))
                     (test-equal "other" (attr ev 'KEY)))
(test-equal "value" (attr ev 'KEY))

;; Test that they are restored on non-local exit
(catch #t
  (lambda ()
   (with-replaced-attrs (ev (KEY "other"))
                        (throw 'any)))
  (lambda _
    (test-equal "value" (attr ev 'KEY))))

(((vcomponent base) attr)
 ((vcomponent) parse-calendar))

(define ev (call-with-input-string
               "BEGIN:VEVENT
KEY:value
END:VEVENT"
             parse-calendar))

(test-assert (eq? #f (attr ev 'MISSING)) )
(test-assert (attr ev 'KEY))
(test-equal "value" (attr ev 'KEY))
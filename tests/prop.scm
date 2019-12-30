(((vcomponent base) prop attr* properties)
 ((vcomponent parse) parse-calendar)
 ((util) sort*))

(define v (call-with-input-string
              "BEGIN:VCOMPONENT
KEY;A=1;B=2:Some text
END:VCOMPONENT"
              parse-calendar))

(test-equal '("1") (prop (attr* v 'KEY) 'A))
(test-equal '("2") (prop (attr* v 'KEY) 'B))
(test-equal #f (prop (attr* v 'KEY) 'C))

(test-equal '(A B) (sort* (map car (properties (attr* v 'KEY)))
                          string<?
                          symbol->string))

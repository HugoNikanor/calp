;;; TODO rename this file to param.scm

(((vcomponent base) param attr* parameters)
 ((vcomponent parse) parse-calendar)
 ((util) sort*))

(define v (call-with-input-string
              "BEGIN:DUMMY
KEY;A=1;B=2:Some text
END:DUMMY"
              parse-calendar))

(test-equal '("1") (param (attr* v 'KEY) 'A))
(test-equal '("2") (param (attr* v 'KEY) 'B))
(test-equal #f (param (attr* v 'KEY) 'C))

(test-equal '(A B) (sort* (map car (parameters (attr* v 'KEY)))
                          string<?
                          symbol->string))

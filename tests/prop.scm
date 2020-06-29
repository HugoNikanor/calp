;;; TODO rename this file to param.scm

(((vcomponent base) param prop* parameters)
 ((vcomponent parse) parse-calendar)
 ((util) sort*))

(define v (call-with-input-string
              "BEGIN:DUMMY
KEY;A=1;B=2:Some text
END:DUMMY"
              parse-calendar))

(test-equal '("1") (param (prop* v 'KEY) 'A))
(test-equal '("2") (param (prop* v 'KEY) 'B))
(test-equal #f (param (prop* v 'KEY) 'C))

(test-equal '(A B) (sort* (map car (parameters (prop* v 'KEY)))
                          string<?
                          symbol->string))

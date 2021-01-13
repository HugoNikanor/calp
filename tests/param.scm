;;; Commentary:
;; Checks that parameters (1) are correctly parsed and stored.
;; (1): 'A', and 'B' in the line "KEY;A=1;B=2:Some text"
;;; Code:

(((vcomponent base) param prop* parameters)
 ((vcomponent parse) parse-calendar)
 ((calp util) sort*))

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

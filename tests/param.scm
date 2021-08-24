;;; Commentary:
;; Checks that parameters (1) are correctly parsed and stored.
;; (1): 'A', and 'B' in the line "KEY;A=1;B=2:Some text"
;;; Code:

(((vcomponent base) param prop* parameters)
 ((vcomponent parse) parse-calendar)
 ((calp util) sort*))

(define v (call-with-input-string
              "BEGIN:DUMMY
X-KEY;A=1;B=2:Some text
END:DUMMY"
              parse-calendar))

(test-equal '("1") (param (prop* v 'X-KEY) 'A))
(test-equal '("2") (param (prop* v 'X-KEY) 'B))
(test-equal #f (param (prop* v 'X-KEY) 'C))

(test-equal '(A B) (sort* (map car (parameters (prop* v 'X-KEY)))
                          string<?
                          symbol->string))

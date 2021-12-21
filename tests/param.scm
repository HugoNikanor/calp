;;; Commentary:
;; Checks that parameters (1) are correctly parsed and stored.
;; (1): 'A', and 'B' in the line "KEY;A=1;B=2:Some text"
;;; Code:

(((vcomponent base) param prop* parameters prop)
 ((vcomponent formats ical parse) parse-calendar)
 ((vcomponent) make-vcomponent)
 ((calp util) sort* set!))

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

;; TODO possibly move this.
;; Checks that a warning is properly raised for
;; unkonwn keys (without an X-prefix)
(test-error
 'warning
 (call-with-input-string "BEGIN:DUMMY
KEY:Some Text
END:DUMMY"))

;; Similar thing happens for sxcal, but during serialization instead
(let ((component (make-vcomponent 'DUMMY)))
  (set! (prop component 'KEY) "Anything")
  (test-error
   'warning
   (vcomponent->sxcal component)))

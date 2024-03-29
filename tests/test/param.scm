;;; Commentary:
;; Checks that parameters (1) are correctly parsed and stored.
;; (1): 'A', and 'B' in the line "KEY;A=1;B=2:Some text"
;;; Code:

(define-module (test param)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent base)
               :select (param prop* parameters prop))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((vcomponent) :select (make-vcomponent))
  :use-module ((hnh util) :select (sort* set!))
  :use-module ((ice-9 ports) :select (call-with-input-string))
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal))
  )

;; TODO clean up this whole test

;; TODO possibly change parsing

(define v
  (call-with-input-string
    "BEGIN:DUMMY
X-KEY;A=1;B=2:Some text
END:DUMMY"
    parse-calendar))

(test-equal '("1") (param (prop* v 'X-KEY) 'A))

(test-equal '("2") (param (prop* v 'X-KEY) 'B))

(test-equal #f (param (prop* v 'X-KEY) 'C))

(test-equal
  '(A B)
  (sort* (map car (parameters (prop* v 'X-KEY)))
         string<?
         symbol->string))


;; TODO possibly move this.
;; Checks that a warning is properly raised for
;; unkonwn keys (without an X-prefix)
(test-error
  'warning
  (call-with-input-string
    "BEGIN:DUMMY
KEY:Some Text
END:DUMMY"
    parse-calendar))

;; Similar thing happens for sxcal, but during serialization instead
(let ((component (make-vcomponent 'DUMMY)))
  (set! (prop component 'KEY) "Anything")
  (test-error
    'warning
    (vcomponent->sxcal component)))



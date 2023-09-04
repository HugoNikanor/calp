;;; Commentary:
;; Checks that parameters (1) are correctly parsed and stored.
;; (1): 'A', and 'B' in the line "KEY;A=1;B=2:Some text"
;;; Code:

(define-module (test param)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent base)
               :select (param prop* parameters prop vline?))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((vcomponent) :select (vcomponent properties set-properties))
  :use-module ((hnh util) :select (sort* set!))
  :use-module ((ice-9 ports) :select (call-with-input-string))
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal))
  )

;; TODO clean up this whole test

;; TODO possibly change parsing

(define v
  (car
   (call-with-input-string
       "BEGIN:DUMMY
X-KEY;A=1;B=2:Some text
END:DUMMY"
     parse-calendar)))

(test-equal '("1") (param (prop* v 'X-KEY) 'A))

(test-equal '("2") (param (prop* v 'X-KEY) 'B))

(test-equal #f (param (prop* v 'X-KEY) 'C))


(test-group "Properties"
 (let ((p (properties v)))
   (test-assert (list? p))
   (test-eqv 1 (length p))
   (test-eq 'X-KEY (caar p))
   (test-assert (vline? (cadar p)))))



;; TODO possibly move this.
;; Checks that a warning is properly raised for
;; unkonwn keys (without an X-prefix)
(test-error "Ensure parse-calendar warns on unknown keys"
  'warning
  (call-with-input-string
    "BEGIN:DUMMY
KEY:Some Text
END:DUMMY"
    parse-calendar))

;; Similar thing happens for sxcal, but during serialization instead
(let ((component (set-properties (vcomponent type: 'DUMMY)
                                 (cons 'KEY "Anything"))))

  (test-error
    'warning
    (vcomponent->sxcal component)))

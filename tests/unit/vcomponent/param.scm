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
  :use-module ((vcomponent) :select (properties set-properties))
  :use-module ((vcomponent create) :select (create-vcomponent with-parameters))
  :use-module ((hnh util) :select (sort* set!))
  :use-module ((ice-9 ports) :select (call-with-input-string))
  )

(define v
  (create-vcomponent 'DUMMY
              x-key: (with-parameters a: "1" b: "2"
                                      "Some text")))

(test-equal '("1") (param (prop* v 'X-KEY) 'A))

(test-equal '("2") (param (prop* v 'X-KEY) 'B))

(test-equal #f (param (prop* v 'X-KEY) 'C))


(test-group "Properties"
 (let ((p (properties v)))
   (test-assert (list? p))
   (test-eqv 1 (length p))
   (test-eq 'X-KEY (caar p))
   (test-assert (vline? (cadar p)))))


'((vcomponent base))

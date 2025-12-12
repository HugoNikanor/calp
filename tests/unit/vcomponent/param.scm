;;; Commentary:
;; Checks that parameters (1) are correctly parsed and stored.
;; (1): 'A', and 'B' in the line "KEY;A=1;B=2:Some text"
;;; Code:

(define-module (test param)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent)
               :select (prop* param* vline?))
  :use-module ((vcomponent) :select (vcomponent-properties))
  :use-module ((vcomponent create) :select (create-vcomponent with-parameters))
  :use-module ((hnh util) :select (sort* set!))
  :use-module (hnh util lens)
  :use-module (hnh util table)
  :use-module (hnh util optional)
  :use-module ((ice-9 ports) :select (call-with-input-string))
  )

(define v
  (create-vcomponent 'DUMMY
              x-key: (with-parameters a: "1" b: "2"
                                      "Some text")))

(test-equal "1"
  (get v (prop* 'X-KEY) just* car* (param* 'A) just*))

(test-equal "2"
  (get v (prop* 'X-KEY) just* car* (param* 'B) just*))

(test-equal (nothing)
  (get/preview v (prop* 'X-KEY) just* car* (param* 'C) just*))


(test-group "Properties"
 (let ((p (vcomponent-properties v)))
   (test-assert (table? p))
   ;; (test-eqv 1 (length p))
   (test-assert (list? (table-get p 'X-KEY)))
   (test-assert (not (null? (table-get p 'X-KEY))))
   (test-assert (vline? (car (table-get p 'X-KEY))))))


'((vcomponent))

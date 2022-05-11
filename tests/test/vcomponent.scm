;;; Commentary:
;; Test that vcomponent parsing works at all.
;;; Code:

(define-module (test vcomponent)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent base)
               :select (prop make-vcomponent add-child! remove-child!
                             children))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar)))

(define ev
  (call-with-input-string
    "BEGIN:DUMMY\nX-KEY:value\nEND:DUMMY"
    parse-calendar))

(test-assert (eq? #f (prop ev 'MISSING)))

(test-assert (prop ev 'X-KEY))

(test-equal "value" (prop ev 'X-KEY))

(define calendar (make-vcomponent 'VCALENDAR))

(add-child! calendar ev)
(test-equal 1 (length (children calendar)))
(remove-child! calendar ev)
(test-equal 0 (length (children calendar)))

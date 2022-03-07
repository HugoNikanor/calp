;;; Commentary:
;; Tests that with-replaced-properties work.
;;; Code:

(define-module (test vcomponent-control)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent util control)
               :select (with-replaced-properties))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((vcomponent base) :select (prop)))

(define ev
  (call-with-input-string
    "BEGIN:DUMMY\nX-KEY:value\nEND:DUMMY"
    parse-calendar))

;; Test that temoraries are set and restored
(test-equal "value" (prop ev 'X-KEY))

(with-replaced-properties
  (ev (X-KEY "other"))
  (test-equal "other" (prop ev 'X-KEY)))

(test-equal "value" (prop ev 'X-KEY))

;; Test that they are restored on non-local exit
(catch #t
       (lambda ()
         (with-replaced-properties
           (ev (X-KEY "other"))
           (throw 'any)))
       (lambda _ (test-equal "value" (prop ev 'X-KEY))))



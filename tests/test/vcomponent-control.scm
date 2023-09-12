;;; Commentary:
;; Tests that with-replaced-properties work.
;;; Code:

(define-module (test vcomponent-control)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (vcomponent create)
  :use-module ((vcomponent util control)
               :select (with-replaced-properties))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((vcomponent base) :select (prop)))

(define ev (vcomponent 'DUMMY x-key: "value"))

(test-group "With replaced properties"
 ;; Test that temoraries are set and restored
  (test-equal "value" (prop ev 'X-KEY))

  (with-replaced-properties
   (ev (X-KEY "other"))
   (test-equal "other" (prop ev 'X-KEY)))

  (test-equal "value" (prop ev 'X-KEY)))

;; Test that they are restored on non-local exit
(test-group "With replaced properties when throwing"
 (catch #t
   (lambda ()
     (with-replaced-properties
      (ev (X-KEY "other"))
      (throw 'any)))
   (lambda _ (test-equal "value" (prop ev 'X-KEY)))))



;;; Commentary:
;; Test that vcomponent parsing works at all.
;;; Code:

(define-module (test vcomponent)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent base)
               :select (prop make-vcomponent reparent! abandon!
                             copy-vcomponent
                             type parent children)))

(define ev
  (let ((ev (make-vcomponent 'DUMMY)))
    (set! (prop ev 'X-KEY) "value")
    ev))

(test-assert (eq? #f (prop ev 'MISSING)))

(test-assert (prop ev 'X-KEY))

(test-equal "value" (prop ev 'X-KEY))

(define calendar (make-vcomponent 'VCALENDAR))

(reparent! calendar ev)
(test-equal 1 (length (children calendar)))
(abandon! calendar ev)
(test-equal 0 (length (children calendar)))


(test-group "Copy VComponent"
  (let ((ev1 (make-vcomponent 'A))
        (ev2 (make-vcomponent 'B))
        (ev3 (make-vcomponent 'C)))
    (set! (prop ev3 'TEST) (list 1 2 3))
    (reparent! ev1 ev2)
    (reparent! ev2 ev3)
    (let* ((ev2* (copy-vcomponent ev2))
           (ev3* (car (children ev2*))))
      ;; NOTE replace this with `vcomponent=?' if that gets written
      (test-group "New object is equivalent to old one"
        (test-equal (type ev2) (type ev2*))
        (test-equal (length (children ev2)) (length (children ev2*))))
      (test-eq ev1 (parent ev2))

      (set! (car (prop ev3* 'TEST)) 10)
      (test-equal "Property values aren't deep copied"
        '(10 2 3) (prop ev3 'TEST))
      (test-equal '(10 2 3) (prop ev3* 'TEST))
      )))

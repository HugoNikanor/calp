;;; Commentary:
;; Basic tests that recurrence rule parsing works.
;; Including that it fails on invalid output.
;;; Code:

(((vcomponent recurrence parse)
  parse-recurrence-rule)
 ((vcomponent recurrence) make-recur-rule)
 ((datetime) mon)
 ((calp util exceptions) warnings-are-errors warning-handler)
 )

(test-equal (make-recur-rule freq: 'HOURLY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=HOURLY"))

(test-equal (make-recur-rule freq: 'HOURLY count: 3 interval: 1 wkst: mon)
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=3"))

(parameterize ((warnings-are-errors #t)
               (warning-handler identity))  ; silence warnings
  (test-error "Invalid FREQ" 'warning
              (parse-recurrence-rule "FREQ=ERR;COUNT=3"))

  (test-error "Negative COUNT" 'warning
              (parse-recurrence-rule "FREQ=HOURLY;COUNT=-1"))

  (test-error "Invalid COUNT"
              'wrong-type-argument
              (parse-recurrence-rule "FREQ=HOURLY;COUNT=err")) )

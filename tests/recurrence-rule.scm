(((vcomponent recurrence parse) parse-recurrence-rule)
 ((vcomponent recurrence internal)
  make-recur-rule weekdays intervals))


(test-equal
    (make-recur-rule (freq 'DAILY) (wkst 'MO) (interval 1))
  (parse-recurrence-rule "FREQ=DAILY"))

(test-equal
    (make-recur-rule (freq 'WEEKLY) (wkst 'MO) (interval 1))
  (parse-recurrence-rule "FREQ=WEEKLY"))

;; TODO more tests

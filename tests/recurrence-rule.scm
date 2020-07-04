(((vcomponent recurrence parse) parse-recurrence-rule)
 ((vcomponent recurrence internal)
  make-recur-rule weekdays intervals)
 ((datetime) mon))


(test-equal
    (make-recur-rule freq: 'DAILY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=DAILY"))

(test-equal
    (make-recur-rule freq: 'WEEKLY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=WEEKLY"))

;; TODO more tests

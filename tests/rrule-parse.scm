(((vcomponent recurrence parse)
  parse-recurrence-rule)
 ((vcomponent recurrence) make-recur-rule)
 ((datetime) mon)
 ((util exceptions) warnings-are-errors warning-handler)
 )

(test-equal (make-recur-rule freq: 'HOURLY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=HOURLY"))

(test-equal (make-recur-rule freq: 'HOURLY count: 3 interval: 1 wkst: mon)
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=3"))

;;; TODO write tests for these cases

(parameterize ((warnings-are-errors #t)
               (warning-handler identity))  ; silence warnings
  (test-error "Invalid FREQ" 'warning
              (parse-recurrence-rule "FREQ=ERR;COUNT=3"))

  (test-error "Negative COUNT" 'warning
              (parse-recurrence-rule "FREQ=HOURLY;COUNT=-1"))

  (test-error "Invalid COUNT"
              'wrong-type-argument
              (parse-recurrence-rule "FREQ=HOURLY;COUNT=err")) )
                                        ; TODO this error seems to have an error
;; => #<<recur-rule> freq: #<<recur-rule> freq: #f until: #f count: #f interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO> until: #f count: 3 interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO>
;; ERR unfulfilled-constraint [ERR] doesn't fulfill constraint of type [FREQ], ignoring

;; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: 1
;;    bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f
;;    byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO>
;; ERR invalid-value [#f] for key [COUNT], ignoring.

;; => #<<recur-rule> freq: HOURLY until: #f count: #<<recur-rule> freq: HOURLY until: #f count: #f interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO> interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO>
;; ERR unfulfilled-constraint [-1] doesn't fulfill constraint of type [COUNT], ignoring

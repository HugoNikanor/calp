(use-modules (vcomponent recurrence parse))

(define-syntax mkrule
  (syntax-rules ()
    ((_ (key val) ...)
     ((record-constructor (@@ (vcomponent recurrence internal) <recur-rule>)
                          (quote (key ...)))
      (quote val) ...))))

(test-equal (mkrule (freq HOURLY) (wkst MO) (interval 1))
  (parse-recurrence-rule "FREQ=HOURLY"))

(test-equal (mkrule (freq HOURLY) (count 3) (interval 1) (wkst MO))
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=3"))

;;; TODO write tests for these cases

(parse-recurrence-rule "FREQ=ERR;COUNT=3")
                                        ; TODO this error seems to have an error
;; => #<<recur-rule> freq: #<<recur-rule> freq: #f until: #f count: #f interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO> until: #f count: 3 interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO>
;; ERR unfulfilled-constraint [ERR] doesn't fulfill constraint of type [FREQ], ignoring

(parse-recurrence-rule "FREQ=HOURLY;COUNT=err")
;; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: 1
;;    bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f
;;    byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO>
;; ERR invalid-value [#f] for key [COUNT], ignoring.

(parse-recurrence-rule "FREQ=HOURLY;COUNT=-1") ; TODO this error seems to have an error
;; => #<<recur-rule> freq: HOURLY until: #f count: #<<recur-rule> freq: HOURLY until: #f count: #f interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO> interval: 1 bysecond: #f byminute: #f byhour: #f byday: #f bymonthday: #f byyearday: #f byweekno: #f bymonth: #f bysetpos: #f wkst: MO>
;; ERR unfulfilled-constraint [-1] doesn't fulfill constraint of type [COUNT], ignoring

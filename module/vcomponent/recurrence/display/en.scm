(define-module (vcomponent recurrence display en)
  :use-module (hnh util)
  :use-module (vcomponent recurrence internal)
  :use-module (text util)
  :use-module (text numbers)
  :use-module (vcomponent recurrence display common)
  :use-module ((datetime) :select (time time->string
                                        datetime->string
                                        week-day-name)))



;; TODO this currently only groups on offsets, but not on days.
;; So 1MO, 1TU becomes "första måndagen och tisdagen", which is good
;; but 1MO, -1MO doesn't become "första och sista måndagen".
;; TODO also, grouping of -dagen. e.g. "första mån- och tisdagen"
(define (format-byday-list lst)
  (let ((groups (group-by car lst)))
    (intersperse
     " as well as "
     (map (lambda (group)
            ;; TODO sort week days
            (case (car group)
              [(#f)
               (list "every "
                     (add-enumeration-punctuation
                      (map (lambda (d) (list (week-day-name (cdr d))))
                           (cadr group)
                           )))]
              [else
               (list (number->string-ordinal (car group)) " "
                     (add-enumeration-punctuation
                      (map (lambda (d) (list (week-day-name (cdr d)) "en"))
                           (cadr group))))])
            )
          groups))))

(define (format-bymonth-day lst)
  (list "the "
        (add-enumeration-punctuation
         (map number->string-ordinal lst))))


(define-public  (format-recurrence-rule rrule)
  (string-trim
   (string-flatten
    (list
     (case (freq rrule)
       [(YEARLY)
        (list (awhen (byday rrule) (list " " (format-byday-list it)))
              (awhen (bymonthday rrule) (list " " (format-bymonth-day it)))
              (awhen (byyearday rrule)
                     (list " day " (add-enumeration-punctuation it)))
              (awhen (bymonth rrule)
                     ;; only `i' here if we have output something else beforehand
                     (list (when (or (byday rrule)
                                     (bymonthday rrule)
                                     (byyearday rrule))
                             " in ")
                           (add-enumeration-punctuation
                            (map rrule-month->string it))))
              (awhen (byweekno rrule)
                     (map (lambda (v) (format #f " v.~a" v)) it))
              )]
       [(MONTHLY)
        (list
         (awhen (byday rrule) (list (format-byday-list it)))
         (awhen (bymonthday rrule) (format-bymonth-day it)))]
       [else '()])

     ;; TODO my parser adds an implicit interval to every object
     ;; this might be wrong
     (cond [(and (eq? 'DAILY (freq rrule)) (= 1 (interval rrule)))
            " daily"]
           [(and (eq? 'YEARLY (freq rrule)) (= 1 (interval rrule)))
            ", yearly"]
           [(and (eq? 'MINUTELY (freq rrule))
                 (zero? (modulo (interval rrule) 15)))
            (list " "
                  (each-string (/ (interval rrule) 15))
                  " 15 minutes")]
           [else
            (list
             " "
             (each-string (interval rrule) (eq? 'YEARLY (freq rrule)))
             " "
             (case (freq rrule)
               ;; p.44 RFC 5545
               [(SECONDLY) "second"]
               [(MINUTELY) "minute"]
               [(HOURLY) "hour"]
               [(DAILY) "day"]

               ;; day offsets CAN ONLY be present when FREQ is
               ;; either MONTHLY or YEARLY
               [(WEEKLY) (aif (byday rrule)
                              (add-enumeration-punctuation
                               (map (compose week-day-name cdr) it))
                              "week")]
               [(MONTHLY) "month"]
               [(YEARLY) "year"]
               [else "ERROR"]
               ))])

     (cond [(and (byminute rrule)
                 (byhour rrule))
            (list
             " at "
             (add-enumeration-punctuation
              (map (lambda (pair)
                     (time->string
                      (time hour: (car pair)
                            minute: (cdr pair))
                      "~H:~M"))
                   (cross-product (byhour rrule)
                                  (byminute rrule)))))]
           [(byhour rrule)
            => (lambda (hours)
                 (list " at " (add-enumeration-punctuation hours)))]
           [else '()])

     (awhen (until rrule)
            (format #f ", until ~a"
                    (datetime->string
                     ;; TODO ordinal on ~d?
                     it "~B ~d, ~Y at ~k:~M")
                    ))
     (cond [(not (count rrule)) ""]
           [(= 1 (count rrule)) (list ", " (count rrule) " time in total")]
           [(count rrule) (list ", " (count rrule) " times in total")]
           [else "ERROR"])))))

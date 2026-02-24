;;; TODO this module is ONLY used by (vcomponent datetime timezone).
;;; Considerer merging it
(define-module (vcomponent type recurrence zic)
  :use-module ((vcomponent type recurrence internal)
               :select (bymonthday byday until recur-rule))
  :use-module (datetime)
  :use-module (datetime zoneinfo)
  :use-module (ice-9 match)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :export (rule->dtstart
           rule->rrule)

  )

;; The first time this rule was/will be applied
(define (rule->dtstart rule)
  ;; NOTE 'minimum and 'maximum represent the begining and end of time.
  ;; since I don't have a way to represent those ideas I just set a very
  ;; high and a very low year here. What 'maximum even entails for a start
  ;; time is not noted in the spec.
  (define d (date year: (case (rule-from rule)
                          ((minimum) 0)
                          ((maximum) 9999)
                          (else (rule-from rule)))
                  month: (rule-in rule)
                  day: 1))

  (define dt
    (datetime
     date: (execute-day-spec d (rule-on rule))
     tz: (case (timespec-type (rule-at rule))
           ((wall) #f)
           ((standard) #f)
           ((utc) "UTC"))))

  (datetime+ dt (seconds->duration (timespec-value (rule-at rule)))))


(define (rule->rrule rule)
  (if (eq? 'only (rule-to rule))
      #f
      (let ((base (recur-rule
                   freq: 'YEARLY
                   interval: 1
                   bymonth: (list (rule-in rule))
                   until: (let ((to  (rule-to rule)))
                            (case to
                              ((maximum) #f)
                              ((minimum) (scm-error 'misc-error "rule->rrule"
                                                    (G_ "Check your input")
                                                    #f #f))
                              (else
                               ;; NOTE I possibly need to check the start of
                               ;; the next rule to know when this rule really
                               ;; ends.
                               (datetime
                                date: (date year: to month: 1 day: 1))))))))


        (match (rule-on rule)
          ((? number? d) (bymonthday base (list d)))
          (('last d)     (byday base (list (cons -1 d))))
          (('< wday base-day) (scm-error 'misc-error "rule->rrule" (G_ "Counting backward for RRULES unsupported") #f #f))
          (('> wday base-day)
           ;; Sun<=25
           ;; Sun>=8
           ;; NOTE this only realy works when base-day = 7n + 1, n ∈ N
           ;; something like Sun>=5 is hard to fix, since we can only
           ;; say which sunday in the month we want (first sunday,
           ;; second sunday, ...).
           (byday base
                  (list
                   (cons (ceiling-quotient base-day 7)
                         wday))))))))

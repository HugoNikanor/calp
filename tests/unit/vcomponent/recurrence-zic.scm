(define-module (test recurrence-zic)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (datetime zoneinfo)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type recurrence zic))



;;; TODO move
(test-group "rule->dtstart"
            (test-equal "last sunday"
              (datetime year: 1967 month: 04 day: 30 hour: 02 minute: 00 second: 00)
              (rule->dtstart
               (zi-rule
                rule-name: 'US
                rule-from: 1967
                rule-to: 1973
                rule-in: 4
                rule-on: '(last 0)
                rule-at: (cons 'wall 7200)
                rule-save: (cons 'daylight 3600)
                rule-letters: "D")))

            (test-equal "sunday >= 1"
              (datetime year: 1977 month: 04 day: 03 hour: 01 minute: 00 second: 00 tz: "UTC")
              (rule->dtstart
               (zi-rule
                rule-name: 'EU
                rule-from: 1977
                rule-to: 1980
                rule-in: 4
                rule-on: `(> ,sun 1)
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'daylight 3600)
                rule-letters: "S")))

            ;; Max and min uses dummy dates, which is slightly wrong
            ;; but shouldn't cause any real problems

            (test-equal "Minimum time"
              (datetime month: 10 day: 30 hour: 1 tz: "UTC")
              (rule->dtstart
               (zi-rule
                rule-name: 'EU
                rule-from: 0
                rule-to: 2000
                rule-in: 10
                rule-on: '(last 0)
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'standard 0)
                rule-letters: "")))
)


;;; TODO move
(test-group "rule->rrule"
            (test-equal "Basic example, and to = maximum"
              (recur-rule
               freq: 'YEARLY interval: 1 wkst: mon
               byday: (list (cons -1 sun))
               bymonth: (list oct))
              (rule->rrule
               (zi-rule
                rule-name: 'EU
                rule-from: 1996
                rule-to: 'maximum
                rule-in: 10
                rule-on: '(last 0)
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'standard 0)
                rule-letters: "")
               ))

            (test-equal "with to = only"
              #f
              (rule->rrule
               (zi-rule
                rule-name: 'EU
                rule-from: 1996
                rule-to: 'only
                rule-in: 10
                rule-on: '(last 2)
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'standard 0)
                rule-letters: "")))

            (test-equal "with definitive to year"
              (recur-rule
               freq: 'YEARLY interval: 1 wkst: mon
               byday: (list (cons -1 tue))
               bymonth: (list oct)
               until: (datetime year: 2000 month: 01 day: 01 hour: 00 minute: 00 second: 00))
              (rule->rrule
               (zi-rule
                rule-name: 'EU
                rule-from: 1996
                rule-to: 2000
                rule-in: 10
                rule-on: '(last 2)
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'standard 0)
                rule-letters: "")))

            (test-equal "on being a month day"
              (recur-rule
               freq: 'YEARLY interval: 1 wkst: mon
               bymonthday: (list 2)
               bymonth: (list oct))
              (rule->rrule
               (zi-rule
                rule-name: 'EU
                rule-from: 1996
                rule-to: 'maximum
                rule-in: 10
                rule-on: 2
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'standard 0)
                rule-letters: "")))

            (test-equal "on being first day after date"
              (recur-rule
               freq: 'YEARLY interval: 1 wkst: mon
               byday: (list (cons 1 mon))
               bymonth: (list oct))
              (rule->rrule
               (zi-rule
                rule-name: 'EU
                rule-from: 1996
                rule-to: 'maximum
                rule-in: 10
                rule-on: `(> ,mon 2)
                rule-at: (cons 'utc 3600)
                rule-save: (cons 'standard 0)
                rule-letters: "")))

            #;
            (test-equal "Crash on counting backwards from date"
              '(misc-error "rule->rrule" "Counting backward for RRULES unsupported" #f #f)
              (catch 'misc-error
                (lambda ()
                 (rule->rrule
                  (zi-rule
                   rule-name: 'EU
                   rule-from: 1996
                   rule-to: 'maximum
                   rule-in: 10
                   rule-on: `(< ,mon 2)
                   rule-at: (cons 'utc 3600)
                   rule-save: (cons 'standard 0)
                   rule-letters: "")))
                list))

            #;
            (test-equal "Crash on to = minimum"
              '(misc-error "rule->rrule" "Check your input" #f #f)
              (catch 'misc-error
                (lambda ()
                  (rule->rrule
                   (zi-rule
                    rule-name: 'EU
                    rule-from: 1996
                    rule-to: 'minimum
                    rule-in: 10
                    rule-on: `(< ,mon 2)
                    rule-at: (cons 'utc 3600)
                    rule-save: (cons 'standard 0)
                    rule-letters: "")))
                list))
            )

'((vcomponent type recurrence zic))

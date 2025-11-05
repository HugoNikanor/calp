;;; Commentary:
;; Tests of recurrence rule generation with focus on correct instances
;; being generated. For tests of basic recurrence functionallity, see
;; recurrence-simple.scm.
;;
;; This file also tests format-recurrence-rule, which checks that human
;; readable representations of the RRULES work.
;;
;; Also contains the tests for EXDATE.
;;
;; Most examples copied from RFC5545, some home written.
;;; Code:

(define-module (test recurrence-advanced)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent type recurrence)
               :select (recur-rule))
  :use-module ((vcomponent type recurrence generate)
               :select (generate-recurrence-set))
  :use-module ((vcomponent type recurrence display)
               :select (format-recurrence-rule))
  :use-module ((vcomponent type recurrence internal)
               :select (count until))
  :use-module ((vcomponent)
               :select (prop1 extract1))
  :use-module (vcomponent create)
  :use-module ((datetime)
               :select (parse-ics-datetime
                        datetime
                        datetime-date
                        time
                        date
                        jan feb mar apr may jun jul aug sep oct nov dec
                        mon tue wed thu fri sat sun
                        datetime->string))
  :use-module ((hnh util) :select (-> set!))
  :use-module ((hnh util env) :select (with-locale1))
  :use-module ((srfi srfi-41) :select (stream->list))
  :use-module ((srfi srfi-88) :select (keyword->string)))

(test-expect-fail "REC: The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months")

(test-expect-fail "STR: The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months")

(test-expect-fail "REC: The second-to-last weekday of the month")

(test-expect-fail "STR: The second-to-last weekday of the month")

;; TODO this test is really slow, figure out why (takes approx. 25s to run)
(test-skip "REC: Every day in January, for 3 years (alt 2)")

(define (run-test comp)
  (test-equal
    (string-append "REC: " (prop1 comp 'SUMMARY))
    (prop1 comp 'X-SET)
    (let ((r (generate-recurrence-set comp)))
      (map (extract1 'DTSTART)
           (if (or (until (prop1 comp 'RRULE))
                   (count (prop1 comp 'RRULE)))
             (stream->list r)
             (stream->list 20 r)))))
  (test-equal
    (string-append "STR: " (prop comp 'SUMMARY))
    (prop comp 'X-SUMMARY)
    ;; NOTE care must be taken so LC_TIME is set to match the parameter to the recurrence rule.
    ;; TODO possibly test with other languages
    (with-locale1
     LC_TIME "sv_SE.UTF-8"
     (lambda ()
       (format-recurrence-rule (prop comp 'RRULE) 'sv)))))

(map run-test
     (list (vevent
             summary:
             "Daily for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'DAILY
              count: 10)
             x-summary:
             "dagligen, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 11 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Daily until December 24, 1997"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'DAILY
              until: (datetime year: 1997 month: 12 day: 24 hour: 00 minute: 00 second: 00 tz: "UTC"))
             x-summary:
             "dagligen, till och med den 24 december, 1997 kl.  0:00"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 23 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every other day - forever"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'DAILY
              interval: 2)
             x-summary:
             "varannan dag"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 10 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every 10 days, 5 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'DAILY
              interval: 10
              count: 5)
             x-summary:
             "var tionde dag, totalt 5 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 12 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every day in January, for 3 years (alt 1)"
             dtstart:
             (datetime year: 1998 month: 01 day: 01 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              until: (datetime year: 2000 month: 01 day: 31 hour: 14 minute: 00 second: 00 tz: "UTC")
              bymonth: (list jan)
              byday: (list sun mon tue wed thu fri sat))
             x-summary:
             "varje lördag, fredag, torsdag, onsdag, tisdag, måndag & söndag i januari, årligen, till och med den 31 januari, 2000 kl. 14:00"
             x-set:
             (list (datetime year: 1998 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 31 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every day in January, for 3 years (alt 2)"
             dtstart:
             (datetime year: 1998 month: 01 day: 01 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'DAILY
              until: (datetime year: 2000 month: 01 day: 31 hour: 14 minute: 00 second: 00 tz: "UTC")
              bymonth: (list jan))
             x-summary:
             "dagligen, till och med den 31 januari, 2000 kl. 14:00"
             x-set:
             (list (datetime year: 1998 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 31 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Weekly for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              count: 10)
             x-summary:
             "varje vecka, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 04 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Weekly until December 24, 1997"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              until: (datetime year: 1997 month: 12 day: 24 hour: 00 minute: 00 second: 00 tz: "UTC"))
             x-summary:
             "varje vecka, till och med den 24 december, 1997 kl.  0:00"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 23 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every other week - forever"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              interval: 2
              wkst: sun)
             x-summary:
             "varannan vecka"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 02 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 02 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 04 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 04 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 26 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Weekly on Tuesday and Thursday for five weeks (alt 1)"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              until: (datetime year: 1997 month: 10 day: 07 hour: 00 minute: 00 second: 00 tz: "UTC")
              wkst: sun
              byday: (list tue thu))
             x-summary:
             "varje tisdag & torsdag, till och med den 07 oktober, 1997 kl.  0:00"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Weekly on Tuesday and Thursday for five weeks (alt 2)"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              count: 10
              wkst: sun
              byday: (list tue thu))
             x-summary:
             "varje tisdag & torsdag, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:"
             dtstart:
             (datetime year: 1997 month: 09 day: 01 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              interval: 2
              until: (datetime year: 1997 month: 12 day: 24 hour: 00 minute: 00 second: 00 tz: "UTC")
              wkst: sun
              byday: (list mon wed fri))
             x-summary:
             "varannan måndag, onsdag & fredag, till och med den 24 december, 1997 kl.  0:00"
             x-set:
             (list (datetime year: 1997 month: 09 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 22 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every other week on Tuesday and Thursday, for 8 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              interval: 2
              count: 8
              wkst: sun
              byday: (list tue thu))
             x-summary:
             "varannan tisdag & torsdag, totalt 8 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 16 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monthly on the first Friday for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              count: 10
              byday: (list (cons 1 fri)))
             x-summary:
             "första fredagen varje månad, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 02 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 04 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 05 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monthly on the first Friday until December 24, 1997"
             dtstart:
             (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              until: (datetime year: 1997 month: 12 day: 24 hour: 00 minute: 00 second: 00 tz: "UTC")
              byday: (list (cons 1 fri)))
             x-summary:
             "första fredagen varje månad, till och med den 24 december, 1997 kl.  0:00"
             x-set:
             (list (datetime year: 1997 month: 09 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 05 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every other month on the first and last Sunday of the month for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 07 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              interval: 2
              count: 10
              byday: (list (cons 1 sun)
                           (cons -1 sun)))
             x-summary:
             "första söndagen samt sista söndagen varannan månad, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 31 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monthly on the second-to-last Monday of the month for 6 months"
             dtstart:
             (datetime year: 1997 month: 09 day: 22 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              count: 6
              byday: (list (cons -2 mon)))
             x-summary:
             "näst sista måndagen varje månad, totalt 6 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 22 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 02 day: 16 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monthly on the third-to-the-last day of the month, forever"
             dtstart:
             (datetime year: 1997 month: 09 day: 28 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              bymonthday: (list -3))
             x-summary:
             "den tredje sista varje månad"
             x-set:
             (list (datetime year: 1997 month: 09 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 02 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 04 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 07 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 08 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 09 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 10 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 11 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 12 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 02 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 04 day: 28 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monthly on the 2nd and 15th of the month for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              count: 10
              bymonthday: (list 2 15))
             x-summary:
             "den andre & femtonde varje månad, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 15 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monthly on the first and last day of the month for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              count: 10
              bymonthday: (list 1 -1))
             x-summary:
             "den förste & sista varje månad, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 01 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every 18 months on the 10th thru 15th of the month for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 10 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              interval: 18
              count: 10
              bymonthday: (list 10 11 12 13 14 15))
             x-summary:
             "den tionde, elfte, tolfte, trettonde, fjortonde & femtonde var artonde månad, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 13 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every Tuesday, every other month"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              interval: 2
              byday: (list tue))
             x-summary:
             "varje tisdag varannan månad"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 12 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Yearly in June and July for 10 occurrences:\n: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY\nonents are specified, the day is gotten from \"DTSTART\""
             dtstart:
             (datetime year: 1997 month: 06 day: 10 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              count: 10
              bymonth: (list 6 7))
             x-summary:
             "juni & juli, årligen, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 06 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 07 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 06 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 07 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 06 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 07 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 06 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 07 day: 10 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every other year on January, February, and March for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 03 day: 10 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              interval: 2
              count: 10
              bymonth: (list jan feb mar))
             x-summary:
             "januari, februari & mars vartannat år, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 03 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 02 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 02 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 03 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 02 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 03 day: 10 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every third year on the 1st, 100th, and 200th day for 10 occurrences"
             dtstart:
             (datetime year: 1997 month: 01 day: 01 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              interval: 3
              count: 10
              byyearday: (list 1 100 200))
             x-summary:
             "dag 1, 100 & 200 vart tredje år, totalt 10 gånger"
             x-set:
             (list (datetime year: 1997 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 04 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 04 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 07 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 01 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 04 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 07 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 01 day: 01 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every 20th Monday of the year, forever"
             dtstart:
             (datetime year: 1997 month: 05 day: 19 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              byday: (list (cons 20 mon)))
             x-summary:
             "tjugonde måndagen, årligen"
             x-set:
             (list (datetime year: 1997 month: 05 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 05 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 05 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2005 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2008 month: 05 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2009 month: 05 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 2010 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2011 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2012 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2013 month: 05 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 2014 month: 05 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2015 month: 05 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 2016 month: 05 day: 16 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monday of week number 20 (where the default start of the week is Monday), forever"
             dtstart:
             (datetime year: 1997 month: 05 day: 12 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              byweekno: (list 20)
              byday: (list mon))
             x-summary:
             "varje måndag v.20, årligen"
             x-set:
             (list (datetime year: 1997 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 05 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2005 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2008 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2009 month: 05 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 2010 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2011 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2012 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2013 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2014 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2015 month: 05 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 2016 month: 05 day: 16 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every Thursday in March, forever"
             dtstart:
             (datetime year: 1997 month: 03 day: 13 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              bymonth: (list mar)
              byday: (list thu))
             x-summary:
             "varje torsdag i mars, årligen"
             x-set:
             (list (datetime year: 1997 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 03 day: 20 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 03 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 03 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 03 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 03 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 03 day: 23 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 03 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 03 day: 01 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 03 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 03 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 03 day: 22 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every Thursday, but only during June, July, and August, forever"
             dtstart:
             (datetime year: 1997 month: 06 day: 05 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              byday: (list thu)
              bymonth: (list 6 7 8))
             x-summary:
             "varje torsdag i juni, juli & augusti, årligen"
             x-set:
             (list (datetime year: 1997 month: 06 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 06 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 06 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 06 day: 26 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 24 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 07 day: 31 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 21 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 28 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 25 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 07 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 07 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 07 day: 16 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every Friday the 13th, forever"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             exdate:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00))
             rrule:
             (recur-rule
              freq: 'MONTHLY
              byday: (list fri)
              bymonthday: (list 13))
             x-summary:
             "varje fredag den trettonde varje månad"
             x-set:
             (list (datetime year: 1998 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 11 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 08 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 04 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 07 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 08 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2005 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 04 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 07 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2008 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2009 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2009 month: 03 day: 13 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "The first Saturday that follows the first Sunday of the month, forever"
             dtstart:
             (datetime year: 1997 month: 09 day: 13 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              byday: (list sat)
              bymonthday: (list 7 8 9 10 11 12 13))
             x-summary:
             "varje lördag den sjunde, åttonde, nionde, tionde, elfte, tolfte & trettonde varje månad"
             x-set:
             (list (datetime year: 1997 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 02 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 04 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 07 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 08 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 09 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 10 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 12 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 09 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 04 day: 10 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every 4 years, the first Tuesday after a Monday in November,\nver (U.S. Presidential Election day)"
             dtstart:
             (datetime year: 1996 month: 11 day: 05 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              interval: 4
              bymonth: (list nov)
              byday: (list tue)
              bymonthday: (list 2 3 4 5 6 7 8))
             x-summary:
             "varje tisdag den andre, tredje, fjärde, femte, sjätte, sjunde eller åttonde i november vart fjärde år"
             x-set:
             (list (datetime year: 1996 month: 11 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 11 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 2008 month: 11 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 2012 month: 11 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 2016 month: 11 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 2020 month: 11 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 2024 month: 11 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 2028 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 2032 month: 11 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 2036 month: 11 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 2040 month: 11 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 2044 month: 11 day: 08 hour: 09 minute: 00 second: 00)
                   (datetime year: 2048 month: 11 day: 03 hour: 09 minute: 00 second: 00)
                   (datetime year: 2052 month: 11 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 2056 month: 11 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 2060 month: 11 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 2064 month: 11 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 2068 month: 11 day: 06 hour: 09 minute: 00 second: 00)
                   (datetime year: 2072 month: 11 day: 08 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months"
             dtstart:
             (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              count: 3
              byday: (list tue wed thu)
              bysetpos: (list 3))
             x-summary:
             "NOT YET IMPLEMENTED"
             x-set:
             (list (datetime year: 1997 month: 09 day: 04 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 07 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 06 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "The second-to-last weekday of the month"
             dtstart:
             (datetime year: 1997 month: 09 day: 29 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              byday: (list mon tue wed thu fri)
              bysetpos: (list -2))
             x-summary:
             "NOT YET IMPLEMENTED"
             x-set:
             (list (datetime year: 1997 month: 09 day: 29 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 10 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 11 day: 27 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 12 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 01 day: 29 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every 3 hours from 9:00 AM to 5:00 PM on a specific day"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'HOURLY
              interval: 3
              until: (datetime year: 1997 month: 09 day: 02 hour: 17 minute: 00 second: 00 tz: "UTC"))
             x-summary:
             "var tredje timme, till och med den 02 september, 1997 kl. 17:00"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 00 second: 00)))
           (vevent
             summary:
             "Every 15 minutes for 6 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MINUTELY
              interval: 15
              count: 6)
             x-summary:
             "varje kvart, totalt 6 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 15 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 30 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 45 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 15 second: 00)))
           (vevent
             summary:
             "Every hour and a half for 4 occurrences"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MINUTELY
              interval: 90
              count: 4)
             x-summary:
             "var sjätte kvart, totalt 4 gånger"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 30 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 30 second: 00)))
           (vevent
             summary:
             "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 1)"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'DAILY
              byhour: (list 9 10 11 12 13 14 15 16)
              byminute: (list 0 20 40))
             x-summary:
             "dagligen kl. 09:00, 09:20, 09:40, 10:00, 10:20, 10:40, 11:00, 11:20, 11:40, 12:00, 12:20, 12:40, 13:00, 13:20, 13:40, 14:00, 14:20, 14:40, 15:00, 15:20, 15:40, 16:00, 16:20 & 16:40"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 20 second: 00)))
           (vevent
             summary:
             "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 2)"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MINUTELY
              interval: 20
              byhour: (list 9 10 11 12 13 14 15 16))
             x-summary:
             "var tjugonde minut kl. 9, 10, 11, 12, 13, 14, 15 & 16"
             x-set:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 20 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 40 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 00 second: 00)
                   (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 20 second: 00)))
           (vevent
             summary:
             "An example where the days generated makes a difference because of WKST"
             dtstart:
             (datetime year: 1997 month: 08 day: 05 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              interval: 2
              count: 4
              byday: (list tue sun)
              wkst: mon)
             x-summary:
             "varannan tisdag & söndag, totalt 4 gånger"
             x-set:
             (list (datetime year: 1997 month: 08 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 24 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "changing only WKST from MO to SU, yields different results.."
             dtstart:
             (datetime year: 1997 month: 08 day: 05 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'WEEKLY
              interval: 2
              count: 4
              byday: (list tue sun)
              wkst: sun)
             x-summary:
             "varannan tisdag & söndag, totalt 4 gånger"
             x-set:
             (list (datetime year: 1997 month: 08 day: 05 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 08 day: 31 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "An example where an invalid date (i.e., February 30) is ignored"
             dtstart:
             (datetime year: 2007 month: 01 day: 15 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'MONTHLY
              bymonthday: (list 15 30)
              count: 5)
             x-summary:
             "den femtonde & trettionde varje månad, totalt 5 gånger"
             x-set:
             (list (datetime year: 2007 month: 01 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 01 day: 30 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 02 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 03 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2007 month: 03 day: 30 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Every Friday & Wednesday the 13th, forever"
             dtstart:
             (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
             exdate:
             (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00))
             rrule:
             (recur-rule
              freq: 'MONTHLY
              byday: (list fri wed)
              bymonthday: (list 13))
             x-summary:
             "varje onsdag & fredag den trettonde varje månad"
             x-set:
             (list (datetime year: 1998 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 11 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 08 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 04 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 07 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 11 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 08 day: 13 hour: 09 minute: 00 second: 00)))
           (vevent
             summary:
             "Monday & Wednesday of week number 20 (where the default start of the week is Monday), forever"
             dtstart:
             (datetime year: 1997 month: 05 day: 12 hour: 09 minute: 00 second: 00)
             rrule:
             (recur-rule
              freq: 'YEARLY
              byweekno: (list 20)
              byday: (list mon wed))
             x-summary:
             "varje onsdag & måndag v.20, årligen"
             x-set:
             (list (datetime year: 1997 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 1997 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 11 hour: 09 minute: 00 second: 00)
                   (datetime year: 1998 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 1999 month: 05 day: 19 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2000 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2001 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                   (datetime year: 2002 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2003 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 05 day: 10 hour: 09 minute: 00 second: 00)
                   (datetime year: 2004 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                   (datetime year: 2005 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                   (datetime year: 2005 month: 05 day: 18 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                   (datetime year: 2006 month: 05 day: 17 hour: 09 minute: 00 second: 00)))
           (vevent
            summary: "Each second, for ever"
            dtstart: (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'SECONDLY)
            x-summary: "varje sekund"
            x-set: (list (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 01)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 02)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 03)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 04)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 05)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 06)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 07)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 08)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 09)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 10)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 11)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 12)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 13)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 14)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 15)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 16)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 17)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 18)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 19)))
           ;; Exdates are applied after rrule's, meaning that less than count
           ;; instances may be present.
           (vevent
            summary: "Exdates are applied AFTER rrule's"
            dtstart: (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'DAILY count: 5)
            exdate: (list (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00))
            x-summary: "dagligen, totalt 5 gånger"
            x-set: (list (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 11 hour: 10 minute: 00 second: 00)
                         ;; (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00) ; skipped by exdate
                         (datetime year: 2022 month: 06 day: 13 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 14 hour: 10 minute: 00 second: 00)
                         ))
           (vevent
            summary: "RDATE:s add to the recurrence rule"
            dtstart: (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'DAILY count: 5)
            rdate: (list (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00))
            x-summary: "dagligen, totalt 5 gånger"
            x-set: (list (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 11 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 13 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 14 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00) ; added by rdate
                         )
            )
           (vevent
            summary: "RDATE:s add to the recurrence rule"
            dtstart: (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'DAILY count: 5)
            exdate: (list (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00))
            rdate: (list (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00))
            x-summary: "dagligen, totalt 5 gånger"
            x-set: (list (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 11 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 13 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 14 hour: 10 minute: 00 second: 00)
                         ;; (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00) ; added by rdate, removed by exdate
                         ))
           ;; TODO rdate with different timezone than dtstart
           ;; TODO rdate with period
           ))



'((vcomponent type recurrence)
  (vcomponent type recurrence generate)
  (vcomponent type recurrence display)
  (vcomponent type recurrence internal))

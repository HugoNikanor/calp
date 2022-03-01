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

;; The human readable tests are expected to fail with any change to the
;; text creator. Proof-read them manually, and update the test cases
;; to match. `x-summary' used for target string. Target strings should
;; be in swedish.

(((vcomponent recurrence parse) parse-recurrence-rule)
 ((vcomponent recurrence generate) generate-recurrence-set)
 ((vcomponent recurrence display) format-recurrence-rule)
 ((vcomponent recurrence internal) count until)
 ((vcomponent base) make-vcomponent prop prop* extract)
 ((datetime) parse-ics-datetime datetime time date
  datetime->string)
 ((hnh util) -> set!)
 ((srfi srfi-41) stream->list)
 ((srfi srfi-88) keyword->string))

(test-expect-fail "RSET: The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months")
(test-expect-fail "STR: The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months")
(test-expect-fail "RSET: The second-to-last weekday of the month")
(test-expect-fail "STR: The second-to-last weekday of the month")

;; TODO this test is really slow, figure out why (takes approx. 25s to run)
(test-skip "RSET: Every day in January, for 3 years (alt 2)")

(define (run-test comp)

  (test-equal (string-append "RSET: " (prop comp 'SUMMARY))
    (prop comp 'X-SET)
    (let ((r (generate-recurrence-set comp)))
      (map (extract 'DTSTART)
           (if (or (until (prop comp 'RRULE))
                   (count (prop comp 'RRULE)))
               (stream->list r)
               (stream->list 20 r)))))

  (test-equal (string-append "STR: " (prop comp 'SUMMARY))
    (prop comp 'X-SUMMARY)
    (format-recurrence-rule (prop comp 'RRULE))))


(define (vevent . rest)
  (define v (make-vcomponent 'VEVENT))

  (let loop ((rem rest))
    (unless (null? rem)
      (let ((symb (-> (car rem)
                      keyword->string
                      string-upcase
                      string->symbol)))
        (set! (prop v symb)
          (case symb
            [(DTSTART EXDATE) (parse-ics-datetime (cadr rem))]
            [(RRULE) (parse-recurrence-rule (cadr rem))]
            [else (cadr rem)]))
        ;; hack for multi valued fields
        (when (eq? symb 'EXDATE)
          (set! (prop* v symb) = list)))
      (loop (cddr rem))))

  v)

(map run-test
 (list
  (vevent
   summary: "Daily for 10 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;COUNT=10"
   x-summary: "dagligen, totalt 10 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-03T09:00:00
                #1997-09-04T09:00:00
                #1997-09-05T09:00:00
                #1997-09-06T09:00:00
                #1997-09-07T09:00:00
                #1997-09-08T09:00:00
                #1997-09-09T09:00:00
                #1997-09-10T09:00:00
                #1997-09-11T09:00:00))

  (vevent
   summary: "Daily until December 24, 1997"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;UNTIL=19971224T000000Z"
   x-summary: "dagligen, till och med den 24 december, 1997 kl.  0:00"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-03T09:00:00
                #1997-09-04T09:00:00
                #1997-09-05T09:00:00
                #1997-09-06T09:00:00
                #1997-09-07T09:00:00
                #1997-09-08T09:00:00
                #1997-09-09T09:00:00
                #1997-09-10T09:00:00
                #1997-09-11T09:00:00
                #1997-09-12T09:00:00
                #1997-09-13T09:00:00
                #1997-09-14T09:00:00
                #1997-09-15T09:00:00
                #1997-09-16T09:00:00
                #1997-09-17T09:00:00
                #1997-09-18T09:00:00
                #1997-09-19T09:00:00
                #1997-09-20T09:00:00
                #1997-09-21T09:00:00
                #1997-09-22T09:00:00
                #1997-09-23T09:00:00
                #1997-09-24T09:00:00
                #1997-09-25T09:00:00
                #1997-09-26T09:00:00
                #1997-09-27T09:00:00
                #1997-09-28T09:00:00
                #1997-09-29T09:00:00
                #1997-09-30T09:00:00
                #1997-10-01T09:00:00
                #1997-10-02T09:00:00
                #1997-10-03T09:00:00
                #1997-10-04T09:00:00
                #1997-10-05T09:00:00
                #1997-10-06T09:00:00
                #1997-10-07T09:00:00
                #1997-10-08T09:00:00
                #1997-10-09T09:00:00
                #1997-10-10T09:00:00
                #1997-10-11T09:00:00
                #1997-10-12T09:00:00
                #1997-10-13T09:00:00
                #1997-10-14T09:00:00
                #1997-10-15T09:00:00
                #1997-10-16T09:00:00
                #1997-10-17T09:00:00
                #1997-10-18T09:00:00
                #1997-10-19T09:00:00
                #1997-10-20T09:00:00
                #1997-10-21T09:00:00
                #1997-10-22T09:00:00
                #1997-10-23T09:00:00
                #1997-10-24T09:00:00
                #1997-10-25T09:00:00
                #1997-10-26T09:00:00
                #1997-10-27T09:00:00
                #1997-10-28T09:00:00
                #1997-10-29T09:00:00
                #1997-10-30T09:00:00
                #1997-10-31T09:00:00
                #1997-11-01T09:00:00
                #1997-11-02T09:00:00
                #1997-11-03T09:00:00
                #1997-11-04T09:00:00
                #1997-11-05T09:00:00
                #1997-11-06T09:00:00
                #1997-11-07T09:00:00
                #1997-11-08T09:00:00
                #1997-11-09T09:00:00
                #1997-11-10T09:00:00
                #1997-11-11T09:00:00
                #1997-11-12T09:00:00
                #1997-11-13T09:00:00
                #1997-11-14T09:00:00
                #1997-11-15T09:00:00
                #1997-11-16T09:00:00
                #1997-11-17T09:00:00
                #1997-11-18T09:00:00
                #1997-11-19T09:00:00
                #1997-11-20T09:00:00
                #1997-11-21T09:00:00
                #1997-11-22T09:00:00
                #1997-11-23T09:00:00
                #1997-11-24T09:00:00
                #1997-11-25T09:00:00
                #1997-11-26T09:00:00
                #1997-11-27T09:00:00
                #1997-11-28T09:00:00
                #1997-11-29T09:00:00
                #1997-11-30T09:00:00
                #1997-12-01T09:00:00
                #1997-12-02T09:00:00
                #1997-12-03T09:00:00
                #1997-12-04T09:00:00
                #1997-12-05T09:00:00
                #1997-12-06T09:00:00
                #1997-12-07T09:00:00
                #1997-12-08T09:00:00
                #1997-12-09T09:00:00
                #1997-12-10T09:00:00
                #1997-12-11T09:00:00
                #1997-12-12T09:00:00
                #1997-12-13T09:00:00
                #1997-12-14T09:00:00
                #1997-12-15T09:00:00
                #1997-12-16T09:00:00
                #1997-12-17T09:00:00
                #1997-12-18T09:00:00
                #1997-12-19T09:00:00
                #1997-12-20T09:00:00
                #1997-12-21T09:00:00
                #1997-12-22T09:00:00
                #1997-12-23T09:00:00
                ;; December 24 SHOULD be missing.
                ))


  (vevent
   summary: "Every other day - forever"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;INTERVAL=2"
   x-summary: "varannan dag"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-04T09:00:00
                #1997-09-06T09:00:00
                #1997-09-08T09:00:00
                #1997-09-10T09:00:00
                #1997-09-12T09:00:00
                #1997-09-14T09:00:00
                #1997-09-16T09:00:00
                #1997-09-18T09:00:00
                #1997-09-20T09:00:00
                #1997-09-22T09:00:00
                #1997-09-24T09:00:00
                #1997-09-26T09:00:00
                #1997-09-28T09:00:00
                #1997-09-30T09:00:00
                #1997-10-02T09:00:00
                #1997-10-04T09:00:00
                #1997-10-06T09:00:00
                #1997-10-08T09:00:00
                #1997-10-10T09:00:00))

  (vevent
   summary: "Every 10 days, 5 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;INTERVAL=10;COUNT=5"
   x-summary: "var tionde dag, totalt 5 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-12T09:00:00
                #1997-09-22T09:00:00
                #1997-10-02T09:00:00
                #1997-10-12T09:00:00))

  (vevent
   summary: "Every day in January, for 3 years (alt 1)"
   dtstart: "19980101T090000"
   rrule: "FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA"
   x-summary: "varje lördag, fredag, torsdag, onsdag, tisdag, måndag & söndag i januari, årligen, till och med den 31 januari, 2000 kl. 14:00"
   x-set: (list #1998-01-01T09:00:00
                #1998-01-02T09:00:00
                #1998-01-03T09:00:00
                #1998-01-04T09:00:00
                #1998-01-05T09:00:00
                #1998-01-06T09:00:00
                #1998-01-07T09:00:00
                #1998-01-08T09:00:00
                #1998-01-09T09:00:00
                #1998-01-10T09:00:00
                #1998-01-11T09:00:00
                #1998-01-12T09:00:00
                #1998-01-13T09:00:00
                #1998-01-14T09:00:00
                #1998-01-15T09:00:00
                #1998-01-16T09:00:00
                #1998-01-17T09:00:00
                #1998-01-18T09:00:00
                #1998-01-19T09:00:00
                #1998-01-20T09:00:00
                #1998-01-21T09:00:00
                #1998-01-22T09:00:00
                #1998-01-23T09:00:00
                #1998-01-24T09:00:00
                #1998-01-25T09:00:00
                #1998-01-26T09:00:00
                #1998-01-27T09:00:00
                #1998-01-28T09:00:00
                #1998-01-29T09:00:00
                #1998-01-30T09:00:00
                #1998-01-31T09:00:00
                #1999-01-01T09:00:00
                #1999-01-02T09:00:00
                #1999-01-03T09:00:00
                #1999-01-04T09:00:00
                #1999-01-05T09:00:00
                #1999-01-06T09:00:00
                #1999-01-07T09:00:00
                #1999-01-08T09:00:00
                #1999-01-09T09:00:00
                #1999-01-10T09:00:00
                #1999-01-11T09:00:00
                #1999-01-12T09:00:00
                #1999-01-13T09:00:00
                #1999-01-14T09:00:00
                #1999-01-15T09:00:00
                #1999-01-16T09:00:00
                #1999-01-17T09:00:00
                #1999-01-18T09:00:00
                #1999-01-19T09:00:00
                #1999-01-20T09:00:00
                #1999-01-21T09:00:00
                #1999-01-22T09:00:00
                #1999-01-23T09:00:00
                #1999-01-24T09:00:00
                #1999-01-25T09:00:00
                #1999-01-26T09:00:00
                #1999-01-27T09:00:00
                #1999-01-28T09:00:00
                #1999-01-29T09:00:00
                #1999-01-30T09:00:00
                #1999-01-31T09:00:00
                #2000-01-01T09:00:00
                #2000-01-02T09:00:00
                #2000-01-03T09:00:00
                #2000-01-04T09:00:00
                #2000-01-05T09:00:00
                #2000-01-06T09:00:00
                #2000-01-07T09:00:00
                #2000-01-08T09:00:00
                #2000-01-09T09:00:00
                #2000-01-10T09:00:00
                #2000-01-11T09:00:00
                #2000-01-12T09:00:00
                #2000-01-13T09:00:00
                #2000-01-14T09:00:00
                #2000-01-15T09:00:00
                #2000-01-16T09:00:00
                #2000-01-17T09:00:00
                #2000-01-18T09:00:00
                #2000-01-19T09:00:00
                #2000-01-20T09:00:00
                #2000-01-21T09:00:00
                #2000-01-22T09:00:00
                #2000-01-23T09:00:00
                #2000-01-24T09:00:00
                #2000-01-25T09:00:00
                #2000-01-26T09:00:00
                #2000-01-27T09:00:00
                #2000-01-28T09:00:00
                #2000-01-29T09:00:00
                #2000-01-30T09:00:00
                #2000-01-31T09:00:00))

  (vevent
   summary: "Every day in January, for 3 years (alt 2)"
   dtstart: "19980101T090000"
   rrule: "FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1"
   x-summary: "dagligen, till och med den 31 januari, 2000 kl. 14:00"
   x-set: (list #1998-01-01T09:00:00
                #1998-01-02T09:00:00
                #1998-01-03T09:00:00
                #1998-01-04T09:00:00
                #1998-01-05T09:00:00
                #1998-01-06T09:00:00
                #1998-01-07T09:00:00
                #1998-01-08T09:00:00
                #1998-01-09T09:00:00
                #1998-01-10T09:00:00
                #1998-01-11T09:00:00
                #1998-01-12T09:00:00
                #1998-01-13T09:00:00
                #1998-01-14T09:00:00
                #1998-01-15T09:00:00
                #1998-01-16T09:00:00
                #1998-01-17T09:00:00
                #1998-01-18T09:00:00
                #1998-01-19T09:00:00
                #1998-01-20T09:00:00
                #1998-01-21T09:00:00
                #1998-01-22T09:00:00
                #1998-01-23T09:00:00
                #1998-01-24T09:00:00
                #1998-01-25T09:00:00
                #1998-01-26T09:00:00
                #1998-01-27T09:00:00
                #1998-01-28T09:00:00
                #1998-01-29T09:00:00
                #1998-01-30T09:00:00
                #1998-01-31T09:00:00
                #1999-01-01T09:00:00
                #1999-01-02T09:00:00
                #1999-01-03T09:00:00
                #1999-01-04T09:00:00
                #1999-01-05T09:00:00
                #1999-01-06T09:00:00
                #1999-01-07T09:00:00
                #1999-01-08T09:00:00
                #1999-01-09T09:00:00
                #1999-01-10T09:00:00
                #1999-01-11T09:00:00
                #1999-01-12T09:00:00
                #1999-01-13T09:00:00
                #1999-01-14T09:00:00
                #1999-01-15T09:00:00
                #1999-01-16T09:00:00
                #1999-01-17T09:00:00
                #1999-01-18T09:00:00
                #1999-01-19T09:00:00
                #1999-01-20T09:00:00
                #1999-01-21T09:00:00
                #1999-01-22T09:00:00
                #1999-01-23T09:00:00
                #1999-01-24T09:00:00
                #1999-01-25T09:00:00
                #1999-01-26T09:00:00
                #1999-01-27T09:00:00
                #1999-01-28T09:00:00
                #1999-01-29T09:00:00
                #1999-01-30T09:00:00
                #1999-01-31T09:00:00
                #2000-01-01T09:00:00
                #2000-01-02T09:00:00
                #2000-01-03T09:00:00
                #2000-01-04T09:00:00
                #2000-01-05T09:00:00
                #2000-01-06T09:00:00
                #2000-01-07T09:00:00
                #2000-01-08T09:00:00
                #2000-01-09T09:00:00
                #2000-01-10T09:00:00
                #2000-01-11T09:00:00
                #2000-01-12T09:00:00
                #2000-01-13T09:00:00
                #2000-01-14T09:00:00
                #2000-01-15T09:00:00
                #2000-01-16T09:00:00
                #2000-01-17T09:00:00
                #2000-01-18T09:00:00
                #2000-01-19T09:00:00
                #2000-01-20T09:00:00
                #2000-01-21T09:00:00
                #2000-01-22T09:00:00
                #2000-01-23T09:00:00
                #2000-01-24T09:00:00
                #2000-01-25T09:00:00
                #2000-01-26T09:00:00
                #2000-01-27T09:00:00
                #2000-01-28T09:00:00
                #2000-01-29T09:00:00
                #2000-01-30T09:00:00
                #2000-01-31T09:00:00))

  (vevent
   summary: "Weekly for 10 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;COUNT=10"
   x-summary: "varje vecka, totalt 10 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-09T09:00:00
                #1997-09-16T09:00:00
                #1997-09-23T09:00:00
                #1997-09-30T09:00:00
                #1997-10-07T09:00:00
                #1997-10-14T09:00:00
                #1997-10-21T09:00:00
                #1997-10-28T09:00:00
                #1997-11-04T09:00:00))

  (vevent
   summary: "Weekly until December 24, 1997"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;UNTIL=19971224T000000Z"
   x-summary: "varje vecka, till och med den 24 december, 1997 kl.  0:00"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-09T09:00:00
                #1997-09-16T09:00:00
                #1997-09-23T09:00:00
                #1997-09-30T09:00:00
                #1997-10-07T09:00:00
                #1997-10-14T09:00:00
                #1997-10-21T09:00:00
                #1997-10-28T09:00:00
                #1997-11-04T09:00:00
                #1997-11-11T09:00:00
                #1997-11-18T09:00:00
                #1997-11-25T09:00:00
                #1997-12-02T09:00:00
                #1997-12-09T09:00:00
                #1997-12-16T09:00:00
                #1997-12-23T09:00:00))

  (vevent
   summary: "Every other week - forever"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;WKST=SU"
   x-summary: "varannan vecka"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-16T09:00:00
                #1997-09-30T09:00:00
                #1997-10-14T09:00:00
                #1997-10-28T09:00:00
                #1997-11-11T09:00:00
                #1997-11-25T09:00:00
                #1997-12-09T09:00:00
                #1997-12-23T09:00:00
                #1998-01-06T09:00:00
                #1998-01-20T09:00:00
                #1998-02-03T09:00:00
                #1998-02-17T09:00:00
                #1998-03-03T09:00:00
                #1998-03-17T09:00:00
                #1998-03-31T09:00:00
                #1998-04-14T09:00:00
                #1998-04-28T09:00:00
                #1998-05-12T09:00:00
                #1998-05-26T09:00:00))

  (vevent
   summary: "Weekly on Tuesday and Thursday for five weeks (alt 1)"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH"
   x-summary: "varje tisdag & torsdag, till och med den 07 oktober, 1997 kl.  0:00"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-04T09:00:00
                #1997-09-09T09:00:00
                #1997-09-11T09:00:00
                #1997-09-16T09:00:00
                #1997-09-18T09:00:00
                #1997-09-23T09:00:00
                #1997-09-25T09:00:00
                #1997-09-30T09:00:00
                #1997-10-02T09:00:00))

  (vevent
   summary: "Weekly on Tuesday and Thursday for five weeks (alt 2)"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH"
   x-summary: "varje tisdag & torsdag, totalt 10 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-04T09:00:00
                #1997-09-09T09:00:00
                #1997-09-11T09:00:00
                #1997-09-16T09:00:00
                #1997-09-18T09:00:00
                #1997-09-23T09:00:00
                #1997-09-25T09:00:00
                #1997-09-30T09:00:00
                #1997-10-02T09:00:00))

  (vevent
   summary: "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:"
   dtstart: "19970901T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR"
   x-summary: "varannan måndag, onsdag & fredag, till och med den 24 december, 1997 kl.  0:00"
   x-set: (list #1997-09-01T09:00:00
                #1997-09-03T09:00:00
                #1997-09-05T09:00:00
                #1997-09-15T09:00:00
                #1997-09-17T09:00:00
                #1997-09-19T09:00:00
                #1997-09-29T09:00:00
                #1997-10-01T09:00:00
                #1997-10-03T09:00:00
                #1997-10-13T09:00:00
                #1997-10-15T09:00:00
                #1997-10-17T09:00:00
                #1997-10-27T09:00:00
                #1997-10-29T09:00:00
                #1997-10-31T09:00:00
                #1997-11-10T09:00:00
                #1997-11-12T09:00:00
                #1997-11-14T09:00:00
                #1997-11-24T09:00:00
                #1997-11-26T09:00:00
                #1997-11-28T09:00:00
                #1997-12-08T09:00:00
                #1997-12-10T09:00:00
                #1997-12-12T09:00:00
                #1997-12-22T09:00:00))

  (vevent
   summary: "Every other week on Tuesday and Thursday, for 8 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH"
   x-summary: "varannan tisdag & torsdag, totalt 8 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-04T09:00:00
                #1997-09-16T09:00:00
                #1997-09-18T09:00:00
                #1997-09-30T09:00:00
                #1997-10-02T09:00:00
                #1997-10-14T09:00:00
                #1997-10-16T09:00:00))

  (vevent
   summary: "Monthly on the first Friday for 10 occurrences"
   dtstart: "19970905T090000"
   rrule: "FREQ=MONTHLY;COUNT=10;BYDAY=1FR"
   x-summary: "första fredagen varje månad, totalt 10 gånger"
   x-set: (list #1997-09-05T09:00:00
                #1997-10-03T09:00:00
                #1997-11-07T09:00:00
                #1997-12-05T09:00:00
                #1998-01-02T09:00:00
                #1998-02-06T09:00:00
                #1998-03-06T09:00:00
                #1998-04-03T09:00:00
                #1998-05-01T09:00:00
                #1998-06-05T09:00:00))

  (vevent
   summary: "Monthly on the first Friday until December 24, 1997"
   dtstart: "19970905T090000"
   rrule: "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"
   x-summary: "första fredagen varje månad, till och med den 24 december, 1997 kl.  0:00"
   x-set: (list #1997-09-05T09:00:00
                #1997-10-03T09:00:00
                #1997-11-07T09:00:00
                #1997-12-05T09:00:00))

  (vevent
   summary: "Every other month on the first and last Sunday of the month for 10 occurrences"
   dtstart: "19970907T090000"
   rrule: "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"
   x-summary: "första söndagen samt sista söndagen varannan månad, totalt 10 gånger"
   x-set: (list #1997-09-07T09:00:00
                #1997-09-28T09:00:00
                #1997-11-02T09:00:00
                #1997-11-30T09:00:00
                #1998-01-04T09:00:00
                #1998-01-25T09:00:00
                #1998-03-01T09:00:00
                #1998-03-29T09:00:00
                #1998-05-03T09:00:00
                #1998-05-31T09:00:00))

  (vevent
   summary: "Monthly on the second-to-last Monday of the month for 6 months"
   dtstart: "19970922T090000"
   rrule: "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO"
   x-summary: "näst sista måndagen varje månad, totalt 6 gånger"
   x-set: (list #1997-09-22T09:00:00
                #1997-10-20T09:00:00
                #1997-11-17T09:00:00
                #1997-12-22T09:00:00
                #1998-01-19T09:00:00
                #1998-02-16T09:00:00))

  (vevent
   summary: "Monthly on the third-to-the-last day of the month, forever"
   dtstart: "19970928T090000"
   rrule: "FREQ=MONTHLY;BYMONTHDAY=-3"
   x-summary: "den tredje sista varje månad"
   x-set: (list #1997-09-28T09:00:00
                #1997-10-29T09:00:00
                #1997-11-28T09:00:00
                #1997-12-29T09:00:00
                #1998-01-29T09:00:00
                #1998-02-26T09:00:00
                #1998-03-29T09:00:00
                #1998-04-28T09:00:00
                #1998-05-29T09:00:00
                #1998-06-28T09:00:00
                #1998-07-29T09:00:00
                #1998-08-29T09:00:00
                #1998-09-28T09:00:00
                #1998-10-29T09:00:00
                #1998-11-28T09:00:00
                #1998-12-29T09:00:00
                #1999-01-29T09:00:00
                #1999-02-26T09:00:00
                #1999-03-29T09:00:00
                #1999-04-28T09:00:00))

  (vevent
   summary: "Monthly on the 2nd and 15th of the month for 10 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15"
   x-summary: "den andre & femtonde varje månad, totalt 10 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-15T09:00:00
                #1997-10-02T09:00:00
                #1997-10-15T09:00:00
                #1997-11-02T09:00:00
                #1997-11-15T09:00:00
                #1997-12-02T09:00:00
                #1997-12-15T09:00:00
                #1998-01-02T09:00:00
                #1998-01-15T09:00:00))

  (vevent
   summary: "Monthly on the first and last day of the month for 10 occurrences"
   dtstart: "19970930T090000"
   rrule: "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1"
   x-summary: "den förste & sista varje månad, totalt 10 gånger"
   x-set: (list #1997-09-30T09:00:00
                #1997-10-01T09:00:00
                #1997-10-31T09:00:00
                #1997-11-01T09:00:00
                #1997-11-30T09:00:00
                #1997-12-01T09:00:00
                #1997-12-31T09:00:00
                #1998-01-01T09:00:00
                #1998-01-31T09:00:00
                #1998-03-01T09:00:00))

  (vevent
   summary: "Every 18 months on the 10th thru 15th of the month for 10 occurrences"
   dtstart: "19970910T090000"
   rrule: "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15"
   x-summary: "den tionde, elfte, tolfte, trettonde, fjortonde & femtonde var artonde månad, totalt 10 gånger"
   x-set: (list #1997-09-10T09:00:00
                #1997-09-11T09:00:00
                #1997-09-12T09:00:00
                #1997-09-13T09:00:00
                #1997-09-14T09:00:00
                #1997-09-15T09:00:00
                #1999-03-10T09:00:00
                #1999-03-11T09:00:00
                #1999-03-12T09:00:00
                #1999-03-13T09:00:00))

  (vevent
   summary: "Every Tuesday, every other month"
   dtstart: "19970902T090000"
   rrule: "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU"
   x-summary: "varje tisdag varannan månad"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-09T09:00:00
                #1997-09-16T09:00:00
                #1997-09-23T09:00:00
                #1997-09-30T09:00:00
                #1997-11-04T09:00:00
                #1997-11-11T09:00:00
                #1997-11-18T09:00:00
                #1997-11-25T09:00:00
                #1998-01-06T09:00:00
                #1998-01-13T09:00:00
                #1998-01-20T09:00:00
                #1998-01-27T09:00:00
                #1998-03-03T09:00:00
                #1998-03-10T09:00:00
                #1998-03-17T09:00:00
                #1998-03-24T09:00:00
                #1998-03-31T09:00:00
                #1998-05-05T09:00:00
                #1998-05-12T09:00:00))

  (vevent
   summary: "Yearly in June and July for 10 occurrences:
: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
onents are specified, the day is gotten from \"DTSTART\""
   dtstart: "19970610T090000"
   rrule: "FREQ=YEARLY;COUNT=10;BYMONTH=6,7"
   x-summary: "juni & juli, årligen, totalt 10 gånger"
   x-set: (list #1997-06-10T09:00:00
                #1997-07-10T09:00:00
                #1998-06-10T09:00:00
                #1998-07-10T09:00:00
                #1999-06-10T09:00:00
                #1999-07-10T09:00:00
                #2000-06-10T09:00:00
                #2000-07-10T09:00:00
                #2001-06-10T09:00:00
                #2001-07-10T09:00:00))

  (vevent
   summary: "Every other year on January, February, and March for 10 occurrences"
   dtstart: "19970310T090000"
   rrule: "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3"
   x-summary: "januari, februari & mars vartannat år, totalt 10 gånger"
   x-set: (list #1997-03-10T09:00:00
                #1999-01-10T09:00:00
                #1999-02-10T09:00:00
                #1999-03-10T09:00:00
                #2001-01-10T09:00:00
                #2001-02-10T09:00:00
                #2001-03-10T09:00:00
                #2003-01-10T09:00:00
                #2003-02-10T09:00:00
                #2003-03-10T09:00:00))

  (vevent
   summary: "Every third year on the 1st, 100th, and 200th day for 10 occurrences"
   dtstart: "19970101T090000"
   rrule: "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200"
   x-summary: "dag 1, 100 & 200 vart tredje år, totalt 10 gånger"
   x-set: (list #1997-01-01T09:00:00
                #1997-04-10T09:00:00
                #1997-07-19T09:00:00
                #2000-01-01T09:00:00
                #2000-04-09T09:00:00
                #2000-07-18T09:00:00
                #2003-01-01T09:00:00
                #2003-04-10T09:00:00
                #2003-07-19T09:00:00
                #2006-01-01T09:00:00))

  (vevent
   summary: "Every 20th Monday of the year, forever"
   dtstart: "19970519T090000"
   rrule: "FREQ=YEARLY;BYDAY=20MO"
   x-summary: "tjugonde måndagen, årligen"
   x-set: (list #1997-05-19T09:00:00
                #1998-05-18T09:00:00
                #1999-05-17T09:00:00
                #2000-05-15T09:00:00
                #2001-05-14T09:00:00
                #2002-05-20T09:00:00
                #2003-05-19T09:00:00
                #2004-05-17T09:00:00
                #2005-05-16T09:00:00
                #2006-05-15T09:00:00
                #2007-05-14T09:00:00
                #2008-05-19T09:00:00
                #2009-05-18T09:00:00
                #2010-05-17T09:00:00
                #2011-05-16T09:00:00
                #2012-05-14T09:00:00
                #2013-05-20T09:00:00
                #2014-05-19T09:00:00
                #2015-05-18T09:00:00
                #2016-05-16T09:00:00))

  (vevent
   summary: "Monday of week number 20 (where the default start of the week is Monday), forever"
   dtstart: "19970512T090000"
   rrule: "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO"
   x-summary: "varje måndag v.20, årligen"
   x-set: (list #1997-05-12T09:00:00
                #1998-05-11T09:00:00
                #1999-05-17T09:00:00
                #2000-05-15T09:00:00
                #2001-05-14T09:00:00
                #2002-05-13T09:00:00
                #2003-05-12T09:00:00
                #2004-05-10T09:00:00
                #2005-05-16T09:00:00
                #2006-05-15T09:00:00
                #2007-05-14T09:00:00
                #2008-05-12T09:00:00
                #2009-05-11T09:00:00
                #2010-05-17T09:00:00
                #2011-05-16T09:00:00
                #2012-05-14T09:00:00
                #2013-05-13T09:00:00
                #2014-05-12T09:00:00
                #2015-05-11T09:00:00
                #2016-05-16T09:00:00))

  (vevent
   summary: "Every Thursday in March, forever"
   dtstart: "19970313T090000"
   rrule: "FREQ=YEARLY;BYMONTH=3;BYDAY=TH"
   x-summary: "varje torsdag i mars, årligen"
   x-set: (list #1997-03-13T09:00:00
                #1997-03-20T09:00:00
                #1997-03-27T09:00:00
                #1998-03-05T09:00:00
                #1998-03-12T09:00:00
                #1998-03-19T09:00:00
                #1998-03-26T09:00:00
                #1999-03-04T09:00:00
                #1999-03-11T09:00:00
                #1999-03-18T09:00:00
                #1999-03-25T09:00:00
                #2000-03-02T09:00:00
                #2000-03-09T09:00:00
                #2000-03-16T09:00:00
                #2000-03-23T09:00:00
                #2000-03-30T09:00:00
                #2001-03-01T09:00:00
                #2001-03-08T09:00:00
                #2001-03-15T09:00:00
                #2001-03-22T09:00:00))

  (vevent
   summary: "Every Thursday, but only during June, July, and August, forever"
   dtstart: "19970605T090000"
   rrule: "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8"
   x-summary: "varje torsdag i juni, juli & augusti, årligen"
   x-set: (list #1997-06-05T09:00:00
                #1997-06-12T09:00:00
                #1997-06-19T09:00:00
                #1997-06-26T09:00:00
                #1997-07-03T09:00:00
                #1997-07-10T09:00:00
                #1997-07-17T09:00:00
                #1997-07-24T09:00:00
                #1997-07-31T09:00:00
                #1997-08-07T09:00:00
                #1997-08-14T09:00:00
                #1997-08-21T09:00:00
                #1997-08-28T09:00:00
                #1998-06-04T09:00:00
                #1998-06-11T09:00:00
                #1998-06-18T09:00:00
                #1998-06-25T09:00:00
                #1998-07-02T09:00:00
                #1998-07-09T09:00:00
                #1998-07-16T09:00:00))

  (vevent
   summary: "Every Friday the 13th, forever"
   dtstart: "19970902T090000"
   exdate: "19970902T090000"
   rrule: "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13"
   x-summary: "varje fredag den trettonde varje månad"
   x-set: (list #1998-02-13T09:00:00
                #1998-03-13T09:00:00
                #1998-11-13T09:00:00
                #1999-08-13T09:00:00
                #2000-10-13T09:00:00
                #2001-04-13T09:00:00
                #2001-07-13T09:00:00
                #2002-09-13T09:00:00
                #2002-12-13T09:00:00
                #2003-06-13T09:00:00
                #2004-02-13T09:00:00
                #2004-08-13T09:00:00
                #2005-05-13T09:00:00
                #2006-01-13T09:00:00
                #2006-10-13T09:00:00
                #2007-04-13T09:00:00
                #2007-07-13T09:00:00
                #2008-06-13T09:00:00
                #2009-02-13T09:00:00
                #2009-03-13T09:00:00))

  (vevent
   summary: "The first Saturday that follows the first Sunday of the month, forever"
   dtstart: "19970913T090000"
   rrule: "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13"
   x-summary: "varje lördag den sjunde, åttonde, nionde, tionde, elfte, tolfte & trettonde varje månad"
   x-set: (list #1997-09-13T09:00:00
                #1997-10-11T09:00:00
                #1997-11-08T09:00:00
                #1997-12-13T09:00:00
                #1998-01-10T09:00:00
                #1998-02-07T09:00:00
                #1998-03-07T09:00:00
                #1998-04-11T09:00:00
                #1998-05-09T09:00:00
                #1998-06-13T09:00:00
                #1998-07-11T09:00:00
                #1998-08-08T09:00:00
                #1998-09-12T09:00:00
                #1998-10-10T09:00:00
                #1998-11-07T09:00:00
                #1998-12-12T09:00:00
                #1999-01-09T09:00:00
                #1999-02-13T09:00:00
                #1999-03-13T09:00:00
                #1999-04-10T09:00:00))

  (vevent
   summary:
   "Every 4 years, the first Tuesday after a Monday in November,
ver (U.S. Presidential Election day)"
   dtstart: "19961105T090000"
   rrule: "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8"
   x-summary: "varje tisdag den andre, tredje, fjärde, femte, sjätte, sjunde eller åttonde i november vart fjärde år"
   x-set: (list #1996-11-05T09:00:00
                #2000-11-07T09:00:00
                #2004-11-02T09:00:00
                #2008-11-04T09:00:00
                #2012-11-06T09:00:00
                #2016-11-08T09:00:00
                #2020-11-03T09:00:00
                #2024-11-05T09:00:00
                #2028-11-07T09:00:00
                #2032-11-02T09:00:00
                #2036-11-04T09:00:00
                #2040-11-06T09:00:00
                #2044-11-08T09:00:00
                #2048-11-03T09:00:00
                #2052-11-05T09:00:00
                #2056-11-07T09:00:00
                #2060-11-02T09:00:00
                #2064-11-04T09:00:00
                #2068-11-06T09:00:00
                #2072-11-08T09:00:00))

  (vevent
   summary: "The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months"
   dtstart: "19970904T090000"
   rrule: "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3"
   x-summary: "NOT YET IMPLEMENTED"
   x-set: (list #1997-09-04T09:00:00
                #1997-10-07T09:00:00
                #1997-11-06T09:00:00))

  (vevent
   summary: "The second-to-last weekday of the month"
   dtstart: "19970929T090000"
   rrule: "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2"
   x-summary: "NOT YET IMPLEMENTED"
   x-set: (list #1997-09-29T09:00:00
                #1997-10-30T09:00:00
                #1997-11-27T09:00:00
                #1997-12-30T09:00:00
                #1998-01-29T09:00:00))

  (vevent
   summary: "Every 3 hours from 9:00 AM to 5:00 PM on a specific day"
   dtstart: "19970902T090000"
   rrule: "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z"
   x-summary: "var tredje timme, till och med den 02 september, 1997 kl. 17:00"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-02T12:00:00
                #1997-09-02T15:00:00))

  (vevent
   summary: "Every 15 minutes for 6 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=MINUTELY;INTERVAL=15;COUNT=6"
   x-summary: "varje kvart, totalt 6 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-02T09:15:00
                #1997-09-02T09:30:00
                #1997-09-02T09:45:00
                #1997-09-02T10:00:00
                #1997-09-02T10:15:00))

  (vevent
   summary: "Every hour and a half for 4 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=MINUTELY;INTERVAL=90;COUNT=4"
   x-summary: "var sjätte kvart, totalt 4 gånger"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-02T10:30:00
                #1997-09-02T12:00:00
                #1997-09-02T13:30:00))

  (vevent
   summary: "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 1)"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40"
   x-summary: "dagligen kl. 09:00, 09:20, 09:40, 10:00, 10:20, 10:40, 11:00, 11:20, 11:40, 12:00, 12:20, 12:40, 13:00, 13:20, 13:40, 14:00, 14:20, 14:40, 15:00, 15:20, 15:40, 16:00, 16:20 & 16:40"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-02T09:20:00
                #1997-09-02T09:40:00
                #1997-09-02T10:00:00
                #1997-09-02T10:20:00
                #1997-09-02T10:40:00
                #1997-09-02T11:00:00
                #1997-09-02T11:20:00
                #1997-09-02T11:40:00
                #1997-09-02T12:00:00
                #1997-09-02T12:20:00
                #1997-09-02T12:40:00
                #1997-09-02T13:00:00
                #1997-09-02T13:20:00
                #1997-09-02T13:40:00
                #1997-09-02T14:00:00
                #1997-09-02T14:20:00
                #1997-09-02T14:40:00
                #1997-09-02T15:00:00
                #1997-09-02T15:20:00))

  (vevent
   summary: "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 2)"
   dtstart: "19970902T090000"
   rrule: "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16"
   x-summary: "var tjugonde minut kl. 9, 10, 11, 12, 13, 14, 15 & 16"
   x-set: (list #1997-09-02T09:00:00
                #1997-09-02T09:20:00
                #1997-09-02T09:40:00
                #1997-09-02T10:00:00
                #1997-09-02T10:20:00
                #1997-09-02T10:40:00
                #1997-09-02T11:00:00
                #1997-09-02T11:20:00
                #1997-09-02T11:40:00
                #1997-09-02T12:00:00
                #1997-09-02T12:20:00
                #1997-09-02T12:40:00
                #1997-09-02T13:00:00
                #1997-09-02T13:20:00
                #1997-09-02T13:40:00
                #1997-09-02T14:00:00
                #1997-09-02T14:20:00
                #1997-09-02T14:40:00
                #1997-09-02T15:00:00
                #1997-09-02T15:20:00))

  (vevent
   summary: "An example where the days generated makes a difference because of WKST"
   dtstart: "19970805T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO"
   x-summary: "varannan tisdag & söndag, totalt 4 gånger"
   x-set: (list #1997-08-05T09:00:00
                #1997-08-10T09:00:00
                #1997-08-19T09:00:00
                #1997-08-24T09:00:00))

  (vevent
   summary: "changing only WKST from MO to SU, yields different results.."
   dtstart: "19970805T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU"
   x-summary: "varannan tisdag & söndag, totalt 4 gånger"
   x-set: (list #1997-08-05T09:00:00
                #1997-08-17T09:00:00
                #1997-08-19T09:00:00
                #1997-08-31T09:00:00))

  (vevent
   summary: "An example where an invalid date (i.e., February 30) is ignored"
   dtstart: "20070115T090000"
   rrule: "FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5"
   x-summary: "den femtonde & tretionde varje månad, totalt 5 gånger"
   x-set: (list #2007-01-15T09:00:00
                #2007-01-30T09:00:00
                #2007-02-15T09:00:00
                #2007-03-15T09:00:00
                #2007-03-30T09:00:00))



  ;; End of examples from RFC, start of own examples

  (vevent
   summary: "Every Friday & Wednesday the 13th, forever"
   dtstart: "19970902T090000"
   exdate: "19970902T090000"
   rrule: "FREQ=MONTHLY;BYDAY=FR,WE;BYMONTHDAY=13"
   x-summary: "varje onsdag & fredag den trettonde varje månad"
   x-set: (list #1998-02-13T09:00:00
                #1998-03-13T09:00:00
                #1998-05-13T09:00:00
                #1998-11-13T09:00:00
                #1999-01-13T09:00:00
                #1999-08-13T09:00:00
                #1999-10-13T09:00:00
                #2000-09-13T09:00:00
                #2000-10-13T09:00:00
                #2000-12-13T09:00:00
                #2001-04-13T09:00:00
                #2001-06-13T09:00:00
                #2001-07-13T09:00:00
                #2002-02-13T09:00:00
                #2002-03-13T09:00:00
                #2002-09-13T09:00:00
                #2002-11-13T09:00:00
                #2002-12-13T09:00:00
                #2003-06-13T09:00:00
                #2003-08-13T09:00:00))

  (vevent
   summary: "Monday & Wednesday of week number 20 (where the default start of the week is Monday), forever"
   dtstart: "19970512T090000"
   rrule: "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO,WE"
   x-summary: "varje onsdag & måndag v.20, årligen"
   x-set: (list #1997-05-12T09:00:00
                #1997-05-14T09:00:00
                #1998-05-11T09:00:00
                #1998-05-13T09:00:00
                #1999-05-17T09:00:00
                #1999-05-19T09:00:00
                #2000-05-15T09:00:00
                #2000-05-17T09:00:00
                #2001-05-14T09:00:00
                #2001-05-16T09:00:00
                #2002-05-13T09:00:00
                #2002-05-15T09:00:00
                #2003-05-12T09:00:00
                #2003-05-14T09:00:00
                #2004-05-10T09:00:00
                #2004-05-12T09:00:00
                #2005-05-16T09:00:00
                #2005-05-18T09:00:00
                #2006-05-15T09:00:00
                #2006-05-17T09:00:00))))

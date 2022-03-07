;;; Commentary:
;; Simples tests of recurrence system, ensuring that all parsers and
;; basic generators work. Some more fully-featured tests are here, but
;; most are instead in recurrence-advanced.scm.
;;; Code:

(define-module (test recurrence-simple)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module ((srfi srfi-41)
               :select (stream-take stream-map stream->list stream-car))
  :use-module ((datetime) :select (day-stream mon))
  :use-module ((vcomponent base) :select (extract prop))
  :use-module ((hnh util exceptions)
               :select (warnings-are-errors warning-handler))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((vcomponent recurrence)
               :select (parse-recurrence-rule
                        make-recur-rule
                        generate-recurrence-set)))

;;; Test that basic parsing or recurrence rules work.

(test-equal (make-recur-rule freq: 'HOURLY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=HOURLY"))

(test-equal (make-recur-rule freq: 'HOURLY count: 3 interval: 1 wkst: mon)
  (parse-recurrence-rule "FREQ=HOURLY;COUNT=3"))

;;; Test that recurrence rule parsing fails where appropriate

(parameterize ((warnings-are-errors #t)
               (warning-handler (lambda _ "")))
  (test-error "Invalid FREQ"
    'warning
    (parse-recurrence-rule "FREQ=ERR;COUNT=3"))
  (test-error "Negative COUNT"
    'warning
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=-1"))
  (test-error "Invalid COUNT"
    'wrong-type-argument
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=err")))

;;; Test that basic recurrence works
;;; also see the neighbour test file recurrence.scm for more tests.

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART;VALUE=DATE:20190302
RRULE:FREQ=DAILY
END:VEVENT"
    parse-calendar))

(test-assert "Generate at all"
  (stream-car (generate-recurrence-set ev)))

(test-assert "Generate some"
  (stream->list
    (stream-take 5 (generate-recurrence-set ev))))

(test-equal "Generate First"
  (stream->list
    5
    (stream-map
      (extract 'DTSTART)
      (generate-recurrence-set ev)))
  (stream->list 5 (day-stream (prop ev 'DTSTART))))

;; We run the exact same thing a secound time, since I had an error with
;; that during development.

(test-equal "Generate Again"
  (stream->list
    (stream-take
      5
      (stream-map
        (extract 'DTSTART)
        (generate-recurrence-set ev))))
  (stream->list
    (stream-take 5 (day-stream (prop ev 'DTSTART)))))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART:20190302T100000
RRULE:FREQ=DAILY
END:VEVENT"
    parse-calendar))

(test-assert "daily 10:00"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART:20190302T100000
DTEND:20190302T120000
RRULE:FREQ=DAILY
END:VEVENT"
    parse-calendar))

(test-assert "daily 10-12"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART:20190302T100000
DTEND:20190302T120000
RRULE:FREQ=WEEKLY
END:VEVENT"
    parse-calendar))

(test-assert "weekly 10-12"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART;TZID=Europe/Stockholm:20190302T100000
DTEND;TZID=Europe/Stockholm:20190302T120000
RRULE:FREQ=WEEKLY
END:VEVENT"
    parse-calendar))

(test-assert "weekly TZ 10-12"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART;TZID=Europe/Stockholm:20190302T100000
DTEND;TZID=Europe/Stockholm:20190302T120000
RRULE:FREQ=WEEKLY
SEQUENCE:1
END:VEVENT"
    parse-calendar))

(test-assert "weekly TZ SEQUENCE 10-12"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART;TZID=Europe/Stockholm:20190302T100000
RRULE:FREQ=WEEKLY
DTEND;TZID=Europe/Stockholm:20190302T120000
SEQUENCE:1
LOCATION:Here
END:VEVENT"
    parse-calendar))

(test-assert "weekly TZ SEQUENCE LOCATION 10-12"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART:20180117T170000
RRULE:FREQ=WEEKLY
LOCATION:~
END:VEVENT"
    parse-calendar))

(test-assert "Just location"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART;TZID=Europe/Stockholm:20180117T170000
DTEND;TZID=Europe/Stockholm:20180117T200000
RRULE:FREQ=WEEKLY
END:VEVENT"
    parse-calendar))

(test-assert "Same times"
  (stream-car (generate-recurrence-set ev)))

(define ev
  (call-with-input-string
    "BEGIN:VEVENT
DTSTART;TZID=Europe/Stockholm:20180117T170000
RRULE:FREQ=WEEKLY
DTEND;TZID=Europe/Stockholm:20180117T200000
SEQUENCE:1
LOCATION:~
END:VEVENT"
    parse-calendar))

;; errer in dtend ?

(test-assert "Full test"
  (stream-car (generate-recurrence-set ev)))

;;; Tests that exceptions (in the recurrence-id meaning)
;;; in recurrence sets are handled correctly.
;;; TODO Is however far from done.

(define uid (symbol->string (gensym "areallyuniqueid")))

;; TODO standardize vcomponents for tests as xcal, for example:
`(vcalendar
   (children
     (vevent
       (properties
         (summary
           (text "Changing type on Recurrence-id."))
         (uid (text ,uid))
         (dtstart (date "20090127"))))
     (vevent
       (properties
         (summary
           (text "Changing type on Recurrence-id."))
         (uid (text ,uid))
         (dtstart
           (params (TZID "Europe/Stockholm"))
           (date-time "20100127T120000"))
         (recurrence-id (date "20100127"))
         (summary
           "This instance only has a time component")))))

(define ev
  (call-with-input-string
    (format
      #f
      "BEGIN:VCALENDAR
BEGIN:VEVENT
SUMMARY:Changing type on Recurrence-id.
UID:~a
DTSTART;VALUE=DATE:20090127
END:VEVENT
BEGIN:VEVENT
UID:~a
SUMMARY:Changing type on Recurrence-id.
DTSTART;TZID=Europe/Stockholm:20100127T120000
RECURRENCE-ID;VALUE=DATE:20100127
SUMMARY:This instance only has a time component
END:VEVENT
END:VCALENDAR"
      uid
      uid)
    parse-calendar))

(test-assert "Changing type on Recurrence id."
  (stream->list 10 (generate-recurrence-set ev)))

;;; Earlier I failed to actually parse the recurrence parts, in short, 1 ≠ "1".

(test-assert "Test that xcal recur rules are parseable"
  ((@@ (vcomponent formats xcal parse) handle-value)
   'recur
   'props-are-unused-for-recur
   '((freq "WEEKLY") (interval "1") (wkst "MO"))))

(define ev
  (sxcal->vcomponent
    '(vevent
       (properties
         (summary (text "reptest"))
         (dtend (date-time "2021-01-13T02:00:00"))
         (dtstart (date-time "2021-01-13T01:00:00"))
         (uid (text "RNW198S6QANQPV1C4FDNFH6ER1VZX6KXEYNB"))
         (rrule (recur (freq "WEEKLY")
                       (interval "1")
                       (wkst "MO")))
         (dtstamp (date-time "2021-01-13T01:42:20Z"))
         (sequence (integer "0")))
       (components))))

(test-assert
  "Check that recurrence rule commint from xcal also works"
  (generate-recurrence-set ev))


;;; TODO test here, for byday parsing, and multiple byday instances in one recur element
;;; TODO which should also test serializing and deserializing to xcal.
;;; For example, the following rules specify every workday

;; BEGIN:VCALENDAR
;; PRODID:-//hugo//calp 0.6.1//EN
;; VERSION:2.0
;; CALSCALE:GREGORIAN
;; BEGIN:VEVENT
;; SUMMARY:Lunch
;; DTSTART:20211129T133000
;; DTEND:20211129T150000
;; LAST-MODIFIED:20211204T220944Z
;; UID:3d82c73c-6cdb-4799-beba-5f1d20d55347
;; RRULE:FREQ=DAILY;BYDAY=MO,TU,WE,TH,FR
;; END:VEVENT
;; END:VCALENDAR

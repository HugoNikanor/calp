;;; Commentary:
;; Simples tests of recurrence system, ensuring that all parsers and
;; basic generators work. Some more fully-featured tests are here, but
;; most are instead in recurrence-advanced.scm.
;;; Code:

(((srfi srfi-41) stream-take stream-map stream->list stream-car)
 ((datetime) day-stream mon)
 ((vcomponent base) extract prop)

 ((calp util exceptions) warnings-are-errors warning-handler)
 ((guile) format)

 ((vcomponent) parse-calendar)
 ((vcomponent xcal parse) sxcal->vcomponent)
 ((vcomponent recurrence)
  parse-recurrence-rule
  make-recur-rule
  generate-recurrence-set))

;;; Test that basic parsing or recurrence rules work.

(test-equal (make-recur-rule freq: 'HOURLY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=HOURLY"))

(test-equal (make-recur-rule freq: 'HOURLY count: 3 interval: 1 wkst: mon)
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=3"))

;;; Test that recurrence rule parsing fails where appropriate

(parameterize ((warnings-are-errors #t)
               (warning-handler identity))  ; silence warnings
  (test-error "Invalid FREQ" 'warning
              (parse-recurrence-rule "FREQ=ERR;COUNT=3"))

  (test-error "Negative COUNT" 'warning
              (parse-recurrence-rule "FREQ=HOURLY;COUNT=-1"))

  (test-error "Invalid COUNT"
              'wrong-type-argument
              (parse-recurrence-rule "FREQ=HOURLY;COUNT=err")) )

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
  (stream->list (stream-take 5 (generate-recurrence-set ev))))

(test-equal "Generate First"
  (stream->list
   5 (stream-map (extract 'DTSTART)
                 (generate-recurrence-set ev)))
  (stream->list
   5 (day-stream
      (prop ev 'DTSTART))))

;; We run the exact same thing a secound time, since I had an error with
;; that during development.

(test-equal "Generate Again"
  (stream->list
   (stream-take
    5 (stream-map (extract 'DTSTART)
                  (generate-recurrence-set ev))))
  (stream->list
   (stream-take
    5 (day-stream
       (prop ev 'DTSTART)))))


(define ev
  (call-with-input-string
      "BEGIN:VEVENT
DTSTART:20190302T100000
RRULE:FREQ=DAILY
END:VEVENT"
    parse-calendar) )

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
         (summary (text "Changing type on Recurrence-id."))
         (uid (text ,uid))
         (dtstart (date "20090127"))))
     (vevent
       (properties
         (summary (text "Changing type on Recurrence-id."))
         (uid (text ,uid))
         (dtstart (params (TZID "Europe/Stockholm")) 
                  (date-time "20100127T120000"))
         (recurrence-id (date "20100127"))
         (summary "This instance only has a time component")))))

(define ev
 (call-with-input-string
     (format #f "BEGIN:VCALENDAR
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
             uid uid)
   parse-calendar))


(test-assert "Changing type on Recurrence id."
  (stream->list 10 (generate-recurrence-set ev)))

;;; Earlier I failed to actually parse the recurrence parts, in short, 1 ≠ "1".

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

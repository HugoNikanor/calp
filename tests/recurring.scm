(((srfi srfi-41) stream-take stream-map stream->list stream-car)
 ((datetime util) day-stream)
 ((vcomponent base) extract attr)

 ((vcomponent) parse-calendar)
 ((vcomponent recurrence) generate-recurrence-set))

;;; Test that basic recurrence works

(define ev
  (call-with-input-string
      "BEGIN:VEVENT
DTSTART:20190302
RRULE:FREQ=DAILY
END:VEVENT"
    parse-calendar))

(test-assert "Generate at all"
  (stream-car (generate-recurrence-set ev)))

(test-assert "Generate some"
  (stream->list (stream-take 5 (generate-recurrence-set ev))))

(test-equal "Generate First"
  (stream->list
   (stream-take
    5 (stream-map (extract 'DTSTART)
                  (generate-recurrence-set ev))))
  (stream->list
   (stream-take
    5 (day-stream
       (attr ev 'DTSTART)))))

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
       (attr ev 'DTSTART)))))


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

;;; TODO, also test:
;;; - limited repetition
;;; - weird rules

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

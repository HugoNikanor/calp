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
  :use-module ((sxml namespaced) :select (sxml->namespaced-sxml))
  :use-module ((calp namespaces) :select (xcal))
  :use-module ((hnh util) :select (->))
  :use-module (datetime)
  :use-module ((vcomponent create) :select (vcalendar vevent with-parameters))
  :use-module ((hnh util exceptions)
               :select (warnings-are-errors warning-handler))
  :use-module ((vcomponent recurrence)
               :select (parse-recurrence-rule
                        recur-rule
                        generate-recurrence-set)))

;;; TODO many of these tests simply check that the procedures don't crash.
;;; Actually check outputs.


;;; Test that basic parsing or recurrence rules work.

(test-equal (recur-rule freq: 'HOURLY wkst: mon interval: 1)
  (parse-recurrence-rule "FREQ=HOURLY"))

(test-equal (recur-rule freq: 'HOURLY count: 3 interval: 1 wkst: mon)
  (parse-recurrence-rule "FREQ=HOURLY;COUNT=3"))

;;; Test that recurrence rule parsing fails where appropriate

(parameterize ((warnings-are-errors #t)
               (warning-handler (lambda _ "")))
  (test-error "Invalid FREQ"
    'wrong-type-arg
    (parse-recurrence-rule "FREQ=ERR;COUNT=3"))
  (test-error "Negative COUNT"
    'wrong-type-arg
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=-1"))
  (test-error "Invalid COUNT"
    'wrong-type-arg
    (parse-recurrence-rule "FREQ=HOURLY;COUNT=err")))

;;; Test that basic recurrence works
;;; also see the neighbour test file recurrence.scm for more tests.

(let ((ev (vevent
           dtstart: (date year: 2029 month: mars day: 2)
           rrule: (recur-rule freq: 'DAILY))))

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
     (stream-take 5 (day-stream (prop ev 'DTSTART))))))

(let ((ev (vevent
           dtstart: (datetime year: 2019 month: mars day: 2 hour: 10)
           rrule: (recur-rule freq: 'DAILY))))
  (test-assert "daily 10:00"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (datetime year: 2019 month: mars day: 2 hour: 10)
           dtend: (datetime year: 2019 month: mars day: 2 hour: 12)
           rrule: (recur-rule freq: 'DAILY))))
  (test-assert "daily 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (datetime year: 2019 month: mars day: 2 hour: 10)
           dtend: (datetime year: 2019 month: mars day: 2 hour: 12)
           rrule: (recur-rule freq: 'WEEKLY))))
  (test-assert "weekly 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
         dtstart: (with-parameters tzid: "Europe/Stockholm"
                                   (datetime year: 2019 month: mars day: 2 hour: 10))
         dtend:   (with-parameters tzid: "Europe/Stockholm"
                                   (datetime year: 2019 month: mars day: 2 hour: 12))
         rrule: (recur-rule freq: 'WEEKLY))))
  (test-assert "weekly TZ 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 10))
           dtend:   (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 12))
           rrule: (recur-rule freq: 'WEEKLY)
           sequence: 1)))
  (test-assert "weekly TZ SEQUENCE 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 10))
           dtend:   (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 12))
           rrule: (recur-rule freq: 'WEEKLY)
           location: "Here"
           sequence: 1)))
  (test-assert "weekly TZ SEQUENCE LOCATION 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (datetime year: 2018 month: jan day: 17 hour: 17)
           rrule: (recur-rule freq: 'WEEKLY)
           location: "~")))
  (test-assert "Just location"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (datetime year: 2018 month: jan day: 17 hour: 17)
           dtend: (datetime year: 2018 month: jan day: 17 hour: 20)
           rrule: (recur-rule freq: 'WEEKLY))))
  (test-assert "Same times"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vevent
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2018 month: jan day: 17 hour: 17))
           dtend:   (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2018 month: jan day: 17 hour: 20))
           rrule: (recur-rule freq: 'WEEKLY))))

  ;; errer in dtend ?
  (test-assert "Full test"
    (stream-car (generate-recurrence-set ev))))

;;; Tests that exceptions (in the recurrence-id meaning)
;;; in recurrence sets are handled correctly.
;;; TODO Is however far from done.

(let* ((uid (symbol->string (gensym "areallyuniqueid")))
       (ev
        (vcalendar
         (list
          (vevent
           summary: "Changing type on Recurrence-id."
           uid: uid
           dtstart: (date year: 2009 month: jan day: 27))
          (vevent
           uid: uid
           summary: "Changing type on Recurrence-id."
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2010 month: jan day: 12 hour: 12))
           summary: "This instance only has a time component)")))))
  (test-assert "Changing type on Recurrence id."
    (stream->list 10 (generate-recurrence-set ev))))

;;; Earlier I failed to actually parse the recurrence parts, in short, 1 ≠ "1".

;;; TODO this should be part of the xCal tests
;; (test-assert "Test that xcal recur rules are parseable"
;;   ((@@ (vcomponent formats xcal parse) handle-value)
;;    'recur
;;    'props-are-unused-for-recur
;;    '((freq "WEEKLY") (interval "1") (wkst "MO"))))

(let ((ev (vevent
           summary: "reptest"
           dtstart: (datetime year: 2021 month: jan day: 13 hour: 1)
           dtend: (datetime year: 2021 month: jan day: 13 hour: 2)
           uid: "RNW198S6QANQPV1C4FDNFH6ER1VZX6KXEYNB"
           rrule: (recur-rule freq: 'WEEKLY
                              interval: 1
                              wkst: monday)
           dtstamp: (datetime year: 2021 month: jan day: 13
                              hour: 1 minute: 42 second: 20
                              tz: "UTC")
           sequence: 0)))

  (test-assert
      "Check that recurrence rule commint from xcal also works"
    (generate-recurrence-set ev)))


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

;; TODO add remaining rules


'((vcomponent recurrence)
  (vcomponent formats ical parse)
  (vcomponent formats xcal parse))

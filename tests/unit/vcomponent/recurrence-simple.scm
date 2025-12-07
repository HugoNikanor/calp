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
  :use-module ((vcomponent) :select (extract1 prop1 vcomponent-children))
  :use-module ((sxml namespaced) :select (sxml->namespaced-sxml))
  :use-module ((calp namespaces) :select (xcal))
  :use-module ((hnh util) :select (->))
  :use-module ((hnh util table) :select (table))
  :use-module (datetime)
  :use-module ((vcomponent create) :select (vcalendar vevent with-parameters))
  :use-module ((hnh util exceptions)
               :select (warnings-are-errors warning-handler))
  :use-module ((vcomponent media-type text calendar parse-semantics)
               :select (parse-recurrence-rule))
  :use-module ((vcomponent type recurrence)
               :select (
                        recur-rule
                        generate-recurrence-set)))

;;; TODO many of these tests simply check that the procedures don't crash.
;;; Actually check outputs.



;;; Test that basic parsing or recurrence rules work.

(test-equal (recur-rule freq: 'HOURLY wkst: mon interval: 1)
  (parse-recurrence-rule (table) "FREQ=HOURLY"))

(test-equal (recur-rule freq: 'HOURLY count: 3 interval: 1 wkst: mon)
  (parse-recurrence-rule (table) "FREQ=HOURLY;COUNT=3"))

;;; Test that recurrence rule parsing fails where appropriate

(test-error "Invalid FREQ"
  'calendar-parse-error
  (parse-recurrence-rule (table) "FREQ=ERR;COUNT=3"))
(test-error "Negative COUNT"
  'calendar-parse-error
  (parse-recurrence-rule (table) "FREQ=HOURLY;COUNT=-1"))
(test-error "Invalid COUNT"
  'calendar-parse-error
  (parse-recurrence-rule (table) "FREQ=HOURLY;COUNT=err"))

;;; Test that basic recurrence works
;;; also see the neighbour test file recurrence.scm for more tests.

(define (vcal . args)
  (vcalendar (list (apply vevent args))))

(let ((ev (vcal
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
      (extract1 'DTSTART)
      (generate-recurrence-set ev)))
    (stream->list 5 (day-stream (prop1 (car (vcomponent-children ev)) 'DTSTART))))

  ;; We run the exact same thing a secound time, since I had an error with
  ;; that during development.

  (test-equal "Generate Again"
    (stream->list
     (stream-take
      5
      (stream-map
       (extract1 'DTSTART)
       (generate-recurrence-set ev))))
    (stream->list
     (stream-take 5 (day-stream (prop1 (car (vcomponent-children ev)) 'DTSTART))))))

(let ((ev (vcal
           dtstart: (datetime year: 2019 month: mars day: 2 hour: 10)
           rrule: (recur-rule freq: 'DAILY))))
  (test-assert "daily 10:00"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (datetime year: 2019 month: mars day: 2 hour: 10)
           dtend: (datetime year: 2019 month: mars day: 2 hour: 12)
           rrule: (recur-rule freq: 'DAILY))))
  (test-assert "daily 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (datetime year: 2019 month: mars day: 2 hour: 10)
           dtend: (datetime year: 2019 month: mars day: 2 hour: 12)
           rrule: (recur-rule freq: 'WEEKLY))))
  (test-assert "weekly 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
         dtstart: (with-parameters tzid: "Europe/Stockholm"
                                   (datetime year: 2019 month: mars day: 2 hour: 10))
         dtend:   (with-parameters tzid: "Europe/Stockholm"
                                   (datetime year: 2019 month: mars day: 2 hour: 12))
         rrule: (recur-rule freq: 'WEEKLY))))
  (test-assert "weekly TZ 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 10))
           dtend:   (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 12))
           rrule: (recur-rule freq: 'WEEKLY)
           sequence: 1)))
  (test-assert "weekly TZ SEQUENCE 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 10))
           dtend:   (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2019 month: mars day: 2 hour: 12))
           rrule: (recur-rule freq: 'WEEKLY)
           location: "Here"
           sequence: 1)))
  (test-assert "weekly TZ SEQUENCE LOCATION 10-12"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (datetime year: 2018 month: jan day: 17 hour: 17)
           rrule: (recur-rule freq: 'WEEKLY)
           location: "~")))
  (test-assert "Just location"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (datetime year: 2018 month: jan day: 17 hour: 17)
           dtend: (datetime year: 2018 month: jan day: 17 hour: 20)
           rrule: (recur-rule freq: 'WEEKLY))))
  (test-assert "Same times"
    (stream-car (generate-recurrence-set ev))))

(let ((ev (vcal
           dtstart: (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2018 month: jan day: 17 hour: 17))
           dtend:   (with-parameters tzid: "Europe/Stockholm"
                                     (datetime year: 2018 month: jan day: 17 hour: 20))
           rrule: (recur-rule freq: 'WEEKLY))))

  ;; errer in dtend ?
  (test-assert "Full test"
    (stream-car (generate-recurrence-set ev))))


;; TODO add remaining rules


'((vcomponent type recurrence)
  (vcomponent type formats ical parse)
  (vcomponent type formats xcal parse))

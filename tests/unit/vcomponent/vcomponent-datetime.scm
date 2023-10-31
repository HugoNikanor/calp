;;; Commentary:
;; Tests that event-clamping (checking how long part of an event
;; overlaps another time span) works.
;;; Code:

(define-module (test vcomponent-datetime)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (hnh util lens)
  :use-module (vcomponent datetime)
  :use-module ((vcomponent create) :select (vevent)))


(test-group "overlapping?"
  (test-assert "date, date"
    (overlapping?
     (vevent summary: "A"
             dtstart: (date year: 2020 month: jan day: 1)
             dtend:   (date year: 2022 month: dec day: 31))
     (vevent summary: "B"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))))

  ;; (test-assert "date, datetime")

  ;; (test-assert "datetime, date")

  ;; (test-assert "datetime, datetime")
  )

(test-group "event-contains?"
  (let* ((dt (datetime year: 2020 month: jan day: 1
                       hour: 10))
         (ev (vevent dtstart: dt
                     dtend: (datetime+ dt (datetime hour: 5)))))
    (test-assert (event-contains? ev dt))
    (test-assert (not (event-contains? ev (set dt datetime-date day 10))))))

(test-group "event-zero-length?"
  (test-assert (not (event-zero-length? (vevent dtstart: (date)))))
  (test-assert (event-zero-length? (vevent dtstart: (datetime))))
  (test-assert (not (event-zero-length? (vevent dtstart: (datetime)
                                               dtend: (datetime))))))

;; (test-group "ev-time<?")
;; (test-group "event-length")

(test-group "event-length/clamped"
 (let ((ev
        (vevent
         dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17)
         dtend:   (datetime year: 2020 month: 4 day:  1 hour: 10))))

   ;; |-----------------| test interval
   ;;                 |----------| event interval

   (test-equal
       "Correct clamping"
     (datetime hour: 7) ; 2020-03-29T17:00 - 2020-03-30T00:00
     (event-length/clamped
      (date year: 2020 month: 3 day: 23) ; a time way before the start of the event
      (date year: 2020 month: 3 day: 29) ; a time slightly after the end of the event
      ev))

   (define utc-ev
     (vevent
      dtstart: (datetime year: 2020 month: 3 day: 29 hour: 15 tz: "UTC")
      dtend:   (datetime year: 2020 month: 4 day:  1 hour:  8 tz: "UTC")))

   (test-equal
       "Correct clamping UTC"
     (datetime hour: 7)
     (event-length/clamped
      (date year: 2020 month: 3 day: 23)
      (date year: 2020 month: 3 day: 29)
      ev))))

;; (test-group "event-length/day")

(test-group "long-event?"
  (test-assert "DTSTART being date is always a long event"
    (long-event? (vevent dtstart: (date))))
  (test-assert "datetime DTSTART without DTEND is always short"
    (not (long-event? (vevent dtstart: (datetime)))))
  (test-assert "Event longer than 24h"
    (not
     (long-event? (vevent dtstart: (datetime year: 2020 month: 1 day: 1 hour: 10)
                          dtend:   (datetime year: 2020 month: 1 day: 1 hour: 20)))))
  (test-assert "Event shorter than 24h"
    (long-event? (vevent dtstart: (datetime year: 2020 month: 1 day: 1
                                            hour: 1)
                         dtend:   (datetime year: 2020 month: 1 day: 2
                                            hour: 1 minute: 1)))))

;; (test-group "really-long-event?")
;; (test-group "final-spanned-time")
;; (test-group "events-between")
;; (test-group "relevant-zone-entry?")
;; (test-group "relevant-zone-rule?")
;; (test-group "zoneinfo->vtimezone")



'((vcomponent datetime))

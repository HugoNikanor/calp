;;; Commentary:
;; Tests that event-clamping (checking how long part of an event
;; overlaps another time span) works.
;;; Code:

(define-module (test vcomponent-datetime)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((datetime) :select (date time datetime))
  :use-module ((vcomponent datetime) :select (event-length/clamped))
  :use-module ((vcomponent create) :select (vevent)))

(define ev
  (vevent
   dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17)
   dtend:   (datetime year: 2020 month: 4 day:  1 hour: 10)))


;; |-----------------| test interval
;;                 |----------| event interval

(test-equal
  "Correct clamping"
  (datetime time: (time hour: 7)) ; 2020-03-29T17:00 - 2020-03-30T00:00
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
  (datetime time: (time hour: 7))
  (event-length/clamped
    (date year: 2020 month: 3 day: 23)
    (date year: 2020 month: 3 day: 29)
    ev))


'((vcomponent datetime))

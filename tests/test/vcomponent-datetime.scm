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
   dtstart: #2020-03-29T17:00:00
   dtend: #2020-04-01T10:00:00))


;; |-----------------| test interval
;;                 |----------| event interval

(test-equal
  "Correct clamping"
  (datetime time: (time hour: 7)) ; 2020-03-29T17:00 - 2020-03-30T00:00
  (event-length/clamped
    #2020-03-23 ; a time way before the start of the event
    #2020-03-29 ; a time slightly after the end of the event
    ev))

(define utc-ev
  (vevent
   dtstart: #2020-03-29T15:00:00Z
   dtend: #2020-04-01T08:00:00Z))

(test-equal
  "Correct clamping UTC"
  (datetime time: (time hour: 7))
  (event-length/clamped
    #2020-03-23
    #2020-03-29
    ev))



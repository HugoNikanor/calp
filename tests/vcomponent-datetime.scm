;;; Commentary:
;; Tests that event-clamping (checking how long part of an event
;; overlaps another time span) works.
;;; Code:

(((datetime)
  date time
  datetime)
 ((vcomponent datetime)
  event-length/clamped)
 ((vcomponent) parse-calendar)
 )

(define ev (call-with-input-string
               "BEGIN:VEVENT
DTSTART:20200329T170000
DTEND:20200401T100000
END:VEVENT"
               parse-calendar))

;; |-----------------| test interval
;;                 |----------| event interval

(test-equal "Correct clamping"
  (datetime time: (time hour: 7)) ; 2020-03-29T17:00 - 2020-03-30T00:00
  (event-length/clamped 
    #2020-03-23 ; a time way before the start of the event
    #2020-03-29 ; a time slightly after the end of the event
    ev))

(define utc-ev (call-with-input-string
               "BEGIN:VEVENT
DTSTART:20200329T150000Z
DTEND:20200401T080000Z
END:VEVENT"
               parse-calendar))

(test-equal "Correct clamping UTC"
  (datetime time: (time hour: 7))
  (event-length/clamped #2020-03-23 #2020-03-29 ev))

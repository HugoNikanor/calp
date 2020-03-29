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


(test-equal "Correct clamping"
  (datetime time: (time hour: 7))
  (event-length/clamped #2020-03-23 #2020-03-29 ev))

(define utc-ev (call-with-input-string
               "BEGIN:VEVENT
DTSTART:20200329T150000Z
DTEND:20200401T080000Z
END:VEVENT"
               parse-calendar))

(test-equal "Correct clamping UTC"
  (datetime time: (time hour: 7))
  (event-length/clamped #2020-03-23 #2020-03-29 ev))

(((srfi srfi-41) stream-take stream-map)
 ((srfi srfi-1) find)
 ((srfi srfi-19 util) day-stream)
 ((guile) make-struct/no-tail)
 ((vcomponent base) children extract type attr)
 ((vcomponent) parse-calendar)
 ((vcomponent recurrence) generate-recurrence-set))

(define cal-1
  (call-with-input-string
      "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-Manual baby!
BEGIN:VEVENT
SUMMARY:Repeating event
DTSTART;20190302T160000
DTEND;VALUE=DATE-TIME:20190302T170000
DTSTAMP;VALUE=DATE-TIME:20190302T165849Z
UID:USG7HSRFJSZ6YURWCNSH3UCKI2PHP19SWGBG
SEQUENCE:0
RRULE:FREQ=DAILY
END:VEVENT
END:VCALENDAR"
    parse-calendar))

(let ((ev (find (lambda (e) (eq? 'VEVENT (type e))) (children cal-1))))
  (test-assert "Generate Recurrence set" (generate-recurrence-set ev))

  (test-equal "Generate First"
    (stream-take 5 (stream-map (extract 'DTSTART)
                               (generate-recurrence-set ev)))
    (stream-take 5 (day-stream (attr ev 'DTSTART))))

  ;; We run the exact same thing a secound time, since I had an error with
  ;; that during development.

  ;; (test-equal "Generate Again"
  ;;   (stream-take 5 (stream-map (extract 'DTSTART)
  ;;                              (generate-recurrence-set ev)))
  ;;   (stream-take 5 (day-stream (attr ev 'DTSTART))))
  )


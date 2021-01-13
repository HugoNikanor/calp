;;; Commentary:
;; Tests that exceptions (in the recurrence-id meaning) 
;; in recurrence sets are handled correctly.
;; TODO Is however far from done.
;;; Code:

(((srfi srfi-41) stream->list)
 ((vcomponent) parse-calendar)
 ((vcomponent recurrence) generate-recurrence-set)
 ((guile) format)
 )

(define uid (symbol->string (gensym "areallyuniqueid")))

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

(((srfi srfi-41) stream-take stream-map stream->list)
 ((srfi srfi-1) find)
 ((srfi srfi-19) date->time-utc time-utc->date)
 ((srfi srfi-19 util) day-stream)
 ((guile) make-struct/no-tail)
 ((vcomponent base) children extract type attr)
 ((vcomponent) parse-calendar)
 ((vcomponent recurrence) generate-recurrence-set))

;;; Test that basic recurrence works

(define ev
  (call-with-input-string
      "BEGIN:VEVENT
DTSTART;20190302
RRULE:FREQ=DAILY
END:VEVENT"
    parse-calendar))

(test-assert "Generate at all"
  (stream->list (stream-take 5 (generate-recurrence-set ev))))

(test-equal "Generate First"
  (stream->list
   (stream-take
    5 (stream-map (extract 'DTSTART)
                  (generate-recurrence-set ev))))
  (stream->list
   (stream-take
    5 (stream-map date->time-utc
                  (day-stream
                   (time-utc->date (attr ev 'DTSTART)))))))

;; We run the exact same thing a secound time, since I had an error with
;; that during development.

(test-equal "Generate Again"
  (stream->list
   (stream-take
    5 (stream-map (extract 'DTSTART)
                  (generate-recurrence-set ev))))
  (stream->list
   (stream-take
    5 (stream-map date->time-utc
                  (day-stream
                   (time-utc->date (attr ev 'DTSTART)))))))


;;; TODO, also test:
;;; - limited repetition
;;; - weird rules

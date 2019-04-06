(define-module (vcalendar datetime)
  #:use-module (vcalendar)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (util)

  #:export (parse-datetime
            event-overlaps?
            event-in?
            ev-time<?)
  )

;;; date time pointer
(define (parse-datetime dtime)
  "Parse the given date[time] string into a date object."
  (let* ((str type (case (string-length dtime)
                     ((8) (values "~Y~m~d" 'all-day))        ; All day
                     ((15) (values "~Y~m~dT~H~M~S" 'local))  ; "local" or TZID-param
                     ((16) (values "~Y~m~dT~H~M~S~z" 'utc))  ; UTC-time
                     ))
         (date (string->date dtime str)))
    (values date type)))

(define (event-overlaps? event begin end)
  "Returns if the event overlaps the timespan.
Event must have the DTSTART and DTEND attribute set."
  (timespan-overlaps? (attr event 'DTSTART)
                      (attr event 'DTEND)
                      begin end))

(define (event-in? ev time)
  "Does event overlap the date that contains time."
  (let* ((date (time-utc->date time))
         (start (date->time-utc (drop-time date)))
         (end (add-day start)))
    (event-overlaps? ev start end)))

(define (ev-time<? a b)
  (time<? (attr a 'DTSTART)
          (attr b 'DTSTART)))

(define-module (vcalendar datetime)
  #:use-module (vcalendar)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)

  #:export (parse-datetime
            event-overlaps?
            event-in?)
  )

(define (parse-datetime dtime)
  "Parse the given date[time] string into a date object."
  ;; localize-date
  (date->time-utc
   (string->date
    dtime
    (case (string-length dtime)
      ((8) "~Y~m~d")
      ((15) "~Y~m~dT~H~M~S")
      ((16) "~Y~m~dT~H~M~S~z")))))

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

(define-public (ev-time<? a b)
  (time<? (attr a 'DTSTART)
          (attr b 'DTSTART)))

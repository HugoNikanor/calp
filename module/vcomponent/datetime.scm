(define-module (vcomponent datetime)
  #:use-module (vcomponent)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (util)

  #:export (parse-datetime
            event-overlaps?
            overlapping?
            event-in?
            ev-time<?)
  )

;;; date time pointer
(define (parse-datetime dtime)
  "Parse the given date[time] string into a date object."
  (string->date
   dtime (case (string-length dtime)
           ((8)  "~Y~m~d")              ; All day
           ((15) "~Y~m~dT~H~M~S")       ; "local" or TZID-param
           ((16) "~Y~m~dT~H~M~S~z"))))  ; UTC-time

(define (event-overlaps? event begin end)
  "Returns if the event overlaps the timespan.
Event must have the DTSTART and DTEND attribute set."
  (timespan-overlaps? (attr event 'DTSTART)
                      (attr event 'DTEND)
                      begin end))

(define (overlapping? event-a event-b)
  (timespan-overlaps? (attr event-a 'DTSTART)
                      (attr event-a 'DTEND)
                      (attr event-b 'DTSTART)
                      (attr event-b 'DTEND)))

(define (event-in? ev time)
  "Does event overlap the date that contains time."
  (let* ((date (time-utc->date time))
         (start (date->time-utc (drop-time date)))
         (end (add-day start)))
    (event-overlaps? ev start end)))

(define (ev-time<? a b)
  (time<? (attr a 'DTSTART)
          (attr b 'DTSTART)))

;; Returns length of the event @var{e}, as a time-duration object.
(define-public (event-length e)
  (time-difference
   (attr e 'DTEND)
   (attr e 'DTSTART)))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
(define-public (event-length/day e start-of-day)
  (time-difference
   (time-min (add-day start-of-day) (attr e 'DTEND))
   (time-max start-of-day (attr e 'DTSTART))))

(define-public (ev-time<? a b)
  (time<? (attr a 'DTSTART)
          (attr b 'DTSTART)))

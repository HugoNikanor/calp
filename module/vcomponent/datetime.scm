(define-module (vcomponent datetime)
  #:use-module (vcomponent base)
  #:use-module (srfi srfi-19 alt)
  #:use-module (srfi srfi-19 alt util)
  #:use-module (util)

  #:export (#;parse-datetime
            event-overlaps?
            overlapping?
            event-contains?
            ev-time<?)
  )

;;; date time pointer
#;
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

(define (event-contains? ev date/-time)
  "Does event overlap the date that contains time."
  (let* ((start (as-date date/-time))
         (end (add-day start)))
    (event-overlaps? ev start end)))

(define-public (ev-time<? a b)
  (date/-time<? (attr a 'DTSTART)
                (attr b 'DTSTART)))

;; Returns length of the event @var{e}, as a time-duration object.
(define-public (event-length e)
  (time-
   (attr e 'DTEND)
   (attr e 'DTSTART)))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
;; currently the secund argument is a date, but should possibly be changed
;; to a datetime to allow for more explicit TZ handling?
(define-public (event-length/day e)
  (time-
   (time-min #00:00:00 (as-time (attr e 'DTEND)))
   (time-max #24:00:00 (as-time (attr e 'DTSTART)))))


;; 22:00 - 03:00
;; 2h för dag 1
;; 3h för dag 2

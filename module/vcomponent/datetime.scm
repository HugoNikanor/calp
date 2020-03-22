(define-module (vcomponent datetime)
  #:use-module (vcomponent base)
  #:use-module (datetime)
  #:use-module (datetime util)
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
  (if (not (attr e 'DTEND))
      (datetime date:
                (if (date? (attr e 'DTSTART))
                    #24:00:00
                    #01:00:00))
      (datetime-difference (as-datetime (attr e 'DTEND))
                           (as-datetime (attr e 'DTSTART)))))

(define-public (event-length/clamped start-date end-date e)
  (datetime-difference (datetime-min (datetime date: (date+ end-date (date day: 1)))   (as-datetime (attr e 'DTEND)))
                       (datetime-max (datetime date: start-date) (as-datetime (attr e 'DTSTART)))))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
;; currently the secund argument is a date, but should possibly be changed
;; to a datetime to allow for more explicit TZ handling?
(define-public (event-length/day date e)
  ;; TODO date= > 2 elements
  (cond [(and (date= (as-date (attr e 'DTSTART))
                 (as-date (attr e 'DTEND)))
              (date= (as-date (attr e 'DTSTART))
                     date))
         (time- (as-time (attr e 'DTEND))
                (as-time (attr e 'DTSTART)))]
        ;; Starts today, end in future day
        [(date= (as-date (attr e 'DTSTART))
                date)
         (time- #24:00:00 (as-time (attr e 'DTSTART)))]
        ;; Ends today, start earlier day
        [(date= (as-date (attr e 'DTEND))
                date)
         (as-time (attr e 'DTEND))]
        ;; start earlier date, end later date
        [else #24:00:00]))


;; 22:00 - 03:00
;; 2h för dag 1
;; 3h för dag 2

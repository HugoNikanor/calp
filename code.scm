#!/usr/bin/guile \
-s
!#

(use-modules (ice-9 format))

(begin
  ;; Supurflous begin block here to make sourcing into geiser easier.
  (setenv "LD_LIBRARY_PATH" (getcwd))
  (load-extension "libguile-calendar" "init_calendar"))

(define v (make-calendar (cadr (command-line))))

(do ((i 0 (1+ i)))
    ((>= i (calendar-size v)))
  (format #t "~3d | ~a~%"
          (1+ i) (car (calendar-get-attr v i "summary"))))


;;; ----------------------------------------

;; (use-modules (srfi srfi-19))

#|
- Z at end means that it's in UTC time.
- No mark at end means that it's in "local time".
- `TZID` can be given as an parameter, specifiying the timezone by name

See p. 46-47 of the RFC
|#

;; (string->date (calendar-get-attr v 0 "dtstart")
;;               "~Y~m~eT~k~M~S~z")
;; => #<date nanosecond: 0 second: 0 minute: 15 hour: 12 day: 29 month: 1 year: 2019 zone-offset: 0>

;; (string->date (calendar-get-attr v 0 "dtstart")
;;               "~Y~m~eT~k~M~S")
;; => #<date nanosecond: 0 second: 0 minute: 15 hour: 12 day: 29 month: 1 year: 2019 zone-offset: 3600>

;; (string-take-right (calendar-get-attr v 0 "dtstart") 1) ; => "Z"

;; (string->date "20180311T133700"
;;               "~Y~m~eT~k~M~S") ; <-- Note missing ~z
;; => #<date nanosecond: 0 second: 0 minute: 37 hour: 13 day: 11 month: 3 year: 2018 zone-offset: 3600>

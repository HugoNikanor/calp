(define-module (datetime arithmetic)
  :use-module ((datetime core)
               :select (
                        date?
                        datetime?
                        (month . date-month)
                        (month* . date-month*)
                        (year . date-year)
                        (year* . date-year*)
                        (day . date-day)
                        (day* . date-day*)
                        (second . time-second)
                        (minute . time-minute)
                        (hour . time-hour)
                        date
                        time
                        datetime
                        datetime-time
                        datetime-date
                        (time* . datetime-time*)
                        (date* . datetime-date*)
                        days-in-month
                        date=
                        date<=
                        leap-year?
                        days-in-year
                        time->seconds
                        ))
  :use-module (datetime duration)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util type)
  :use-module (ice-9 curried-definitions)
  :use-module ((rnrs base) :version (6) :select (assert))
  :export (
           date+
           date-
           ;; time+
           ;; time-
           datetime+/naive
           datetime-/naive

           add-time-duration
           remove-time-duration

           date-difference
           datetime-difference/naive
           )
  )

;;; duration := date-duration | week-duration | time-duration
;;; date± :: date, date-duration | week-duration -> date
;;; time± :: time, time-duration -> time
;;; datetime± :: datetime, duration -> datetime

;;; date := Y / m / d
;;; Y := integer
;;; m := [1, 12]
;;; d := [1, 31] if m in [jan, mar, may, jul, aug, okt, dec]
;;;      [1, 30] if m in [apr, jun, sep, nov]
;;;      28 if m == 2 and not leap year
;;;      29 if m == 2 and leap year
;;; time := H / M / S
;;; H := [0, 23]
;;; M := [0, 59]
;;; S := [0, 59] ; FUCK leap-seconds
;;; datetime := date / time [/ timezone]

;;; Arithmetic is done from largest to smallest part, meaning that #2026-01-30 + P1M1D will be #2026-03-01.
;;; IF it where from smallest to largest then the result would de #2026-02-28

(define (clamp-to-month d)
  (if (> (date-day d) (days-in-month d))
      (date-day d (days-in-month d))
      d))

(define (date-add-months start month-count)
  ;; (typecheck start date?)
  (typecheck month-count (and exact-integer? (not negative?)))

  (let ((o m (floor/ (+ (1- (date-month start)) month-count) 12)))
    (-> start
        (modify date-year* (lambda (y) (+ y o)))
        (set date-month* (1+ m))
        clamp-to-month)))

(define (date-add-days start day-count)
  ;; (typecheck start date?)
  (typecheck day-count (and exact-integer? (not negative?)))

  (let loop ((base start)
             (remaining day-count))
    (let ((days-left-in-month (- (days-in-month base)
                                 (date-day base))))
      (if (> remaining days-left-in-month)
          (loop (-> base
                    (date-day 1)
                    (date-add-months 1))
                (- remaining days-left-in-month 1))
          (modify base date-day* (lambda (d) (+ d remaining)))))))

(define (date+% start dur)
  ;; (typecheck start date?)
  ;; (typecheck dur duration?)

  (if (duration-negative? dur)
      (date-% start (duration-negate dur))
      (-> start
          (modify date-year* (lambda (y) (+ y (duration-year dur))))
          (date-add-months (duration-month dur))
          (date-add-days (duration-day dur)))))


(define (date-remove-months start month-count)
  ;; (typecheck start date?)
  (typecheck month-count (and exact-integer? (not negative?)))

  (let ((o m (floor/ (- (1- (date-month start)) month-count) 12)))
    (-> start
        (modify date-year* (lambda (y) (+ y o)))
        (set date-month* (1+ m))
        clamp-to-month)))

;;; Returns a new date, which is the last day of the month proceeding the given date
;;; Example:
;;; (end-of-previous-month #2026-03-11) ⇒ #2026-02-28
(define (end-of-previous-month dt)
  (if (= 1 (date-month dt))
      (date year: (1- (date-year dt))
            month: 12 day: 31)
      (let ((d* (modify dt date-month* 1-)))
        (set d* date-day* (days-in-month d*)))))

(define (date-remove-days start day-count)
  ;; (typecheck start date?)
  (typecheck day-count (and exact-integer? (not negative?)))

  (let loop ((base start)
             (remaining day-count))
    (if (> (date-day base) remaining)
        (modify base date-day* (lambda (d) (- d remaining)))
        (loop (end-of-previous-month base)
              (- remaining (date-day base))))))

(define (date-% start dur)
  ;; (typecheck start date?)
  ;; (typecheck dur duration?)

  (if (duration-negative? dur)
      (date+% start (duration-negate dur))
      (-> start
          (date-remove-days (duration-day dur))
          (date-remove-months (duration-month dur))
          (modify date-year* (lambda (y) (- y (duration-year dur)))))))

(define (date+ start . durations)
  (fold (swap date+%) start durations))

(define (date- start . durations)
  (fold (swap date-%) start durations))

(define (time+% t d)
  ;; This assumes coninious time (e.g. no DST changes or similar).
  (if (duration-negative? d)
      (time-% t (duration-negate d))
      (let* ((r s (floor/ (+ (time-second t)   (duration-second d)) 60))
             (r m (floor/ (+ (time-minute t) r (duration-minute d)) 60))
             (r h (floor/ (+ (time-hour   t) r (duration-hour   d)) 24)))
        (values (time hour: h minute: m second: s) r))))

(define (time-% t d)
  (if (duration-negative? d)
      (time+% t (duration-negate d))
      (let* ((r s (floor/ (- (time-second t)       (duration-second d)) 60))
             (r m (floor/ (- (time-minute t) (- r) (duration-minute d)) 60))
             (r h (floor/ (- (time-hour   t) (- r) (duration-hour   d)) 24)))
        (values (time hour: h minute: m second: s) r))))

(define ((date-add-or-remove-days days) date)
  (if (negative? days)
      (date-remove-days date (- days))
      (date-add-days date days)))

(define (add-time-duration datetime duration)
  (let ((t r (time+% (datetime-time datetime) duration)))
    (-> datetime
        (modify datetime-date* (date-add-or-remove-days r))
        (set datetime-time* t))))

(define (remove-time-duration datetime duration)
  (let ((t r (time-% (datetime-time datetime) duration)))
    (-> datetime
        (modify datetime-date* (date-add-or-remove-days r))
        (set datetime-time* t))))

(define (datetime+/naive start duration)
  (add-time-duration
   (modify start datetime-date* (lambda (d) (date+% d duration)))
   duration))

(define (datetime-/naive start duration)
  (remove-time-duration
   (modify start datetime-date* (lambda (d) (date-% d duration)))
   duration))


(define (year-day d)
  (typecheck d date?)

  (apply
   +
   (date-day d)
   (map (lambda (month)
          (days-in-month (date year: (date-year d) month: month day: 1)))
        (iota (1- (date-month d)) 1))))

(define (range-non-inclusive-both from to)
  (if (> to from)
      (iota (- to from 1) (1+ from))
      '()))


;; (days-until-new-years-eve #2026-12-31) => 0
(define (days-until-new-years-eve d)
  (- (days-in-year d)
     (year-day d)))

;;; Return a date count, such that a + days-between(a, b) == b
(define (days-between a b)
  (assert (date<= a b))
  (if (= (date-year a) (date-year b))
      (- (year-day b)
         (year-day a))
      (apply +
             (days-until-new-years-eve a)
             (year-day b)
             (map (lambda (y) (leap-year? y) 366 365)
                  (range-non-inclusive-both (date-year a) (date-year b))))))

(define (date-difference b a)
  (if (date<= a b)
      (duration day: (days-between a b))
      (duration day: (days-between b a)
                sign: '-)))

(define (seconds-until-midnight t)
  (- (* 60 60 24)
     (time->seconds t)))

;;; This blindly assumes 24 hours each day.
;;; It tries to count days where possible, but generate a bit to much hours in some cases. For example, 
;; (datetime-difference #2026-01-11T00:00 #2026-01-09T00:00)
;; $29 = #.(string->duration "P1DT24H")
;; ((swap datetime-difference) #2026-01-11T23:59:59 #2026-01-09T00:00:00)
;; $26 = #.(string->duration "-P1DT47H59M59S")

(define (datetime-difference/naive b a)
  (cond ((date= (datetime-date a) (datetime-date b))
         (let ((d (- (time->seconds (datetime-time b))
                     (time->seconds (datetime-time a)))))
           (duration
            sign: (if (negative? d) '- '+)
            second: (abs d))))
        ((date<= (datetime-date a) (datetime-date b))
         (duration
          second:
          (+ (seconds-until-midnight (datetime-time a))
             (time->seconds (datetime-time b)))
          day: (1- (days-between (datetime-date a) (datetime-date b)))))
        (else (duration-negate (datetime-difference/naive a b)))))

;; (datetime-difference #2026-01-10T01:00 #2026-01-09T23:00)
;;; => DT2H

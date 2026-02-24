(define-module (datetime extra)
  :use-module (datetime core)
  :use-module (datetime arithmetic)
  :use-module (datetime duration)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :export (
           start-of-year
           end-of-year
           start-of-week
           end-of-week

           date-stream
           day-stream

           days-in-interval
           year-day

           weeks-in-year
           week-1-start
           week-number
           date-starting-week

           timespan-overlaps?

           date-range

           month-days
           ))

(define (start-of-year date)
  (-> date
      (day 1)
      (month 1)))

(define (end-of-year d)
  (-> (start-of-year d)
      (date+ (duration year: 1))
      (date- (duration day: 1))))

(define (date-stream date-increment start-day)
  (stream-iterate (lambda (d) (date+ d date-increment))
                  start-day))

(define (day-stream start-day)
  (date-stream (duration day: 1) start-day))

;; The amount of days in the given interval, both end points inclusive
(define (days-in-interval start-date end-date)
  (1+ ((@@ (datetime arithmetic) days-between) start-date end-date)))



;; Day from start of the year, so 1 feb would be day 32.
;; Also known as Julian day.
;; (define (year-day date)
;;   (days-in-interval (start-of-year date) date))
(define year-day (@@ (datetime arithmetic) year-day))


(define* (weeks-in-year date optional: (wkst (week-start)))
  (week-number (end-of-year date) wkst))


;; given a date, returns the date the first week of that year starts on.
;; @example
;; (week-1-start #2020-01-01 mon)
;; ⇒ 2019-12-30
;; @end example
(define* (week-1-start d optional: (wkst (week-start)))
  (let* ((ystart (start-of-year d))
         (day-index (modulo (- (week-day ystart) wkst) 7)))
    (if (> day-index 3)
        (date+ ystart (duration day: (- 7 day-index)))
        (date- ystart (duration day: day-index)))))



;; (week-number #2020-01-01 mon) ; => 1
;; (week-number #2019-12-31 mon) ; => 1
(define* (week-number d optional: (wkst (week-start)))
  ;; Calculating week number for starts of week was much simpler.
  ;; We can both skip the special cases for Jan 1, 2 & 3. It also
  ;; solved some weird bug that was here before.

  (let ((d (start-of-week d wkst)))
   (cond
    [(and (= 12 (month d))
          (memv (day d) '(29 30 31))
          (< (year d) (year (date+ (start-of-week d wkst)
                                   (duration day: 3)))))
     1]

    [else
     (let* ((w1-start (week-1-start d wkst))
            (week day (floor/ (days-in-interval w1-start d)
                              7)))
       (1+ week))])))

(define* (date-starting-week
          week-number d
          optional: (wkst (week-start)))
  (date+ (week-1-start d wkst)
         (duration week: (1- week-number))))




;; @verbatim
;;    A          B          C          D          E         ¬F
;; |s1|     :     |s2| : |s1|     :     |s2| :          : |s1|
;; |  |     :     |  | : |  ||s2| : |s1||  | : |s1||s2| : |  |
;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | : |  ||  | :
;;     |  | : |  |     : |  ||  | : |  ||  | : |  ||  | :     |s2|
;;     |  | : |  |     : |  |     :     |  | :          :     |  |
;;
;; Infinitely short ---+|s2| : |s1|+--- : two instants don't overlap
;; events, overlap   s1      :      s2  :
;; @end verbatim
;; 
;; E is covered by both case A and B.
(define (timespan-overlaps? s1-begin s1-end s2-begin s2-end)
  "Return whetever or not two timespans overlap."
  ;; TODO why do we require unzoned or UTC? Wouldn't it be enough that all four datetimes are in the same zone?
  (typecheck s1-begin (or utc-datetime? unzoned-datetime?))
  (typecheck s1-end   (or utc-datetime? unzoned-datetime?))
  (typecheck s2-begin (or utc-datetime? unzoned-datetime?))
  (typecheck s2-end   (or utc-datetime? unzoned-datetime?))
  (unless (equal? (tz s1-begin) (tz s1-end) (tz s2-begin) (tz s2-end))
    (scm-error 'wrong-type-arg "timespan-overlaps?"
               "All datetimes must be UTC or zoneless. Got: [~s, ~s), [~s, ~s)"
               (list s1-begin s1-end s2-begin s2-end) #f))

  ;; TODO isn't this overly complicated?
  ;; Can't we just check if s1-begin is in [s2-begin, s2-end) or
  ;; s1-end is in [s2-begin, s2-end)?

  (or
   ;; A
   (and (datetime< s2-begin s1-end)
        (datetime< s1-begin s2-end))

   ;; B
   (and (datetime< s1-begin s2-end)
        (datetime< s2-begin s1-end))

   ;; C
   (and (datetime<= s1-begin s2-begin)
        (datetime< s2-end s1-end))

   ;; D
   (and (datetime<= s2-begin s1-begin)
        (datetime< s1-end s2-end))))


;; Returns a list of all dates from start to end.
;; both inclusive
;; date, day increment → [list date]
(define* (date-range start end optional: (increment 1))
  (stream->list (ceiling (/ (days-in-interval start end) increment))
                (date-stream (duration day: increment) start)))



;; returns the date the week containing d started.
;; (start-of-week #2020-04-02 sun) ; => 2020-03-29
(define* (start-of-week d optional: (week-start (week-start)))
  (date- d (duration day: (modulo (- (week-day d)
                                     week-start)
                                  7))))

;; (end-of-week #2020-04-01 mon)
;; => 2020-04-05
(define* (end-of-week d optional: (week-start (week-start)))
  (date+ (start-of-week d week-start)
         (duration day: 6)))



;; Given a month and and which day the week starts on,
;; returns three lists, which are:
;; The days leading up to the current month, but share a week
;; The days in the current month
;; The days after the current month, but which shares a week.
;; 
;;       mars 2020
;; må ti on to fr lö sö
;;                    1
;;  2  3  4  5  6  7  8
;;  9 10 11 12 13 14 15
;; 16 17 18 19 20 21 22
;; 23 24 25 26 27 28 29
;; 30 31
;; @lisp
;; (month-days #2020-03-01 mon)
;; ; ⇒ (2020-02-24 ... 2020-02-29)
;; ; ⇒ (2020-03-01 ... 2020-03-31)
;; ; ⇒ (2020-04-01 ... 2020-04-05)
;; @end lisp
;; Ignores day component of @var{date}.
(define* (month-days date* optional: (week-start (week-start)))
  (let* ((month-len (days-in-month date*))
         (prev-month-len (days-in-month (date- date* (duration month: 1))))
         (month-start (modulo (- (week-day date*) week-start) 7)))
    (values
     (map (lambda (d) (-> date* (date- (duration month: 1)) (day d)))
          (iota month-start (1+ (- prev-month-len month-start))))
     (map (lambda (d) (day date* d)) (iota month-len 1))
     (map (lambda (d) (-> date* (date+ (duration month: 1)) (day d)))
          (iota (modulo (- (* 7 5) month-len month-start) 7) 1)))))

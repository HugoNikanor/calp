(define-module (vcalendar recurrence internal)
  #:use-module (util)
  #:use-module (srfi srfi-88)
  #:export (make-recur-rule
            weekdays intervals))

;; Immutable, since I easily want to be able to generate the recurence set for
;; the same event multiple times.
(define-quick-record recur-rule
  (public: freq until count interval bysecond byminute byhour
           byday bymonthday byyearday byweekno bymonth bysetpos
           wkst))

(define (make-recur-rule interval wkst)
  ((record-constructor <recur-rule> '(interval wkst)) interval wkst))

(define weekdays
  '(SU MO TU WE TH FR SA))

(define intervals
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))

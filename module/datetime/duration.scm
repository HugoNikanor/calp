(define-module (datetime duration)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 regex)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (hnh util exceptions)
  :export (
           duration
           duration?

           duration-year   duration-year*
           duration-month  duration-month*
           duration-day    duration-day*
           duration-hour   duration-hour*
           duration-minute duration-minute*
           duration-second duration-second*

           duration-week*
           duration-time*

           duration-negate
           duration-negative?
           duration-positive?

           string->duration
           duration->string))

;;; Further expansion:
;;; - the "biggest" component is allowed fractions, meaning that D0.5Y
;;    (or D0,5Y) is a valid duration


;;; duration± does NOT exist. Operations on pure time, pure week, and
;;; pure date work as expected, but as soon as months or years are
;;; involved no sensible operation exists.

;;; Time durations are implemented as date-time durations with all
;;; date components set to 0.

;;; Times are stored internally as seconds, but ALWAYS displayed
;;; normalized to hour, minute, second tuples.



(define (((focus-quotitient d) n) f)
  (let ((v r (floor/ n d)))
    (+ (* d (f v)) r)))

(define (((focus-remainder d) n) f)
  (let ((v r (floor/ n d)))
    (+ (* d v) (f r))))



(define-type (duration-week
              serializer: (lambda (r) `(string->duration ,(duration-week->string r))))
  (duration-week-week type: (and exact-integer? (not negative?))
                      keyword: week))

(define (duration-week->string dur)
  (format #f "P~aW" (duration-week-week dur)))



(define (duration-date->string dur)
  ;; Both P0D and PT0S are acceptable "zeroes" here
  ;; This currently uses PT0S since the logic is simpler
  (let ((y (duration-date-year dur))
        (m (duration-date-month dur))
        (d (duration-date-day dur))
        (t (duration-date-time dur)))
   (string-append
    "P"
    (if (> y 0) (format #f "~aY" y) "")
    (if (> m 0) (format #f "~aM" m) "")
    (if (> d 0) (format #f "~aD" d) "")

    (cond ((= 0 y m d t) "T0S")
          ((> t 0)
           (let* ((h r (floor/ t 3600))
                  (m s (floor/ r 60)))
             (string-append
              "T"
              (if (> h 0) (format #f "~aH" h) "")
              (if (> m 0) (format #f "~aM" m) "")
              (if (> s 0) (format #f "~aS" s) ""))))
          (else "")))))


(define-type (duration-date
              serialier:
              (lambda (r) `(string->duration ,(duration-date->string r))))
  (duration-date-year   keyword: year default: 0
                        type: (and exact-integer? (not negative?)))
  (duration-date-month  keyword: month default: 0
                        type: (and exact-integer? (not negative?)))
  (duration-date-day    keyword: day  default: 0
                        type: (and exact-integer? (not negative?)))
  (duration-date-time   keyword: time default: 0
                        type: (and exact-integer? (not negative?))))



(define (duration-component? x)
  (or (duration-date? x)
      (duration-week? x)))





(define-type
  (duration
   serializer:
   (lambda (r) `(string->duration ,(duration->string r)))
   constructor:
   (lambda (constructor typecheck)
     (lambda* (key: (sign '+)
                    year month day
                    hour minute second
                    week)
       (define value
         (cond ((and week (or year month day hour minute second))
                (scm-error 'type-error "duration"
                           "#:week can't be mixed with other keys"
                           '() #f))
               (week (duration-week week: week))
               ;; ((not (or year month day))
               ;;  (duration-date time: (+ (* 3600 (or hour   0))
               ;;                          (*   60 (or minute 0))
               ;;                          (or second 0))))
               (else
                (duration-date year:  (or year  0)
                               month: (or month 0)
                               day:   (or day   0)
                               time: (+ (* 3600 (or hour   0))
                                        (*   60 (or minute 0))
                                        (or second 0))))))
       (typecheck sign value)
       (constructor sign value))))
  (duration-sign type: (memv '(+ -)))
  (duration-value type: (or duration-week? duration-date?)))


(define (duration->string dur)
  (typecheck dur duration?)
  (string-append
   (case (duration-sign dur)
     ((+) "") ((-) "-"))
   (let ((d (duration-value dur)))
     (cond ((duration-date? d) (duration-date->string d))
           ((duration-week? d) (duration-week->string d))
           (else (unreachable "duration-type serializer"))))))




(define (duration-negate duration)
  (modify duration duration-sign*
          (lambda (s) (case s ((+) '-) ((-) '+)))))

(define (duration-negative? duration)
  (eq? '- (duration-sign duration)))

(define (duration-positive? duration)
  (eq? '+ (duration-sign duration)))


(define (ensure-date-duration dur)
  (cond ((duration-date? dur) dur)
        ((duration-week? dur)
         (duration-date day: (* 7 (duration-week-week dur))))
        (else (scm-error 'type-error "ensure-date-duration"
                         "Can't cast ~s to a duration-date"
                         (list dur) #f))))

(define ((project-as-date dur) f)
  (f (ensure-date-duration dur)))

(define duration-year*
  (lens-compose duration-value*
                project-as-date
                duration-date-year*))

(define duration-month*
  (lens-compose duration-value*
                project-as-date
                duration-date-month*))

(define duration-day*
  (lens-compose duration-value*
                project-as-date
                duration-date-day*))

(define duration-time*
  (lens-compose duration-value*
                project-as-date
                duration-date-time*))

(define duration-hour*
  (lens-compose duration-time*
                (focus-quotitient 3600)))

(define duration-minute*
  (lens-compose duration-time*
                (focus-remainder 3600)
                (focus-quotitient 60)))

(define duration-second*
  (lens-compose duration-time*
                (focus-remainder 3600)
                (focus-remainder 60)))

;;; NOTE: This ONLY works if the duration was created as a week duration.
;;; An empty duration DOES NOT work, but could be made to work
(define duration-week*
  (lens-compose duration-value*
                duration-week-week*))


(define (duration-year   d) (get d duration-year*))
(define (duration-month  d) (get d duration-month*))
(define (duration-day    d) (get d duration-day*))
(define (duration-hour   d) (get d duration-hour*))
(define (duration-minute d) (get d duration-minute*))
(define (duration-second d) (get d duration-second*))


(define period-date-time-rx
  (make-regexp
   "^([+-])?P([0-9]+Y)?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+S)?)?$"))
(define period-week-rx
  (make-regexp "^([+-])?P([0-9]+)W$"))

(define (string->duration str)
  (cond ((string=? str "P")
         (scm-error 'misc-error "string->duration"
                    "String not parsable as duration: ~s"
                    (list str) #f))
        ((regexp-exec period-date-time-rx str)
         => (lambda (m)
              (define (extract i)
                (cond ((match:substring m i)
                       => (lambda (s)
                            (string->number (string-drop-right s 1))))
                      (else 0)))
              (duration
               sign: (cond ((match:substring m 1)
                            => (lambda (s) (if (string=? s "-") '- '+)))
                           (else '+))
               year:  (extract 2)
               month: (extract 3)
               day:   (extract 4)
               hour:   (extract 6)
               minute: (extract 7)
               second: (extract 8))))
        ((regexp-exec period-week-rx str)
         => (lambda (m)
              (duration
               sign: (cond ((match:substring m 1)
                            => (lambda (s) (if (string=? s "-") '- '+)))
                           (else '+))
               week: (string->number (match:substring m 2)))))
        (else
         (scm-error 'misc-error "string->duration"
                    "String not parsable as duration: ~s"
                    (list str) #f))))

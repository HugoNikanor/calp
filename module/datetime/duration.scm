(define-module (datetime duration)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 curried-definitions)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (hnh util exceptions)
  :export (
           duration
           duration?

           duration-year*
           duration-month*
           duration-day*
           duration-hour*
           duration-minute*
           duration-second*

           ;; duration+
           ;; duration-

           string->duration
           duration->string

           ))

;;; Further expansion:
;;; - the "biggest" component is allowed fractions, meaning that D0.5Y
;;    (or D0,5Y) is a valid duration

#|
dur-value  = (["+"] / "-") "P" (dur-date / dur-time / dur-week) ; ;
                                        ; ;
dur-date   = dur-day [dur-time]         ; ;
dur-time   = "T" (dur-hour / dur-minute / dur-second) ; ;
dur-week   = 1*DIGIT "W"                ; ;
dur-hour   = 1*DIGIT "H" [dur-minute]   ; ;
dur-minute = 1*DIGIT "M" [dur-second]   ; ;
dur-second = 1*DIGIT "S"                ; ;
dur-day    = 1*DIGIT "D"                ; ;
                                        ; ;
|#                                      ;

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


;;; duration± does NOT exist. Operations on pure time, pure week, and
;;; pure date work as expected, but as soon as months or years are
;;; involved no sensible operation exists.

;;; ----------------------------------------


(define-type (duration-week
              serializer: (lambda (r) `(string->duration ,(duration-week->string r))))
  (duration-week-week type: (and exact-integer? (not negative?))
                      keyword: week))

(define (duration-week->string dur)
  (format #f "P~aW" (duration-week-week dur)))

;;; ----------------------------------------

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
  (duration-date-year   keyword: year  default: 0 type: (and exact-integer? (not negative?)))
  (duration-date-month  keyword: month default: 0 type: (and exact-integer? (not negative?)))
  (duration-date-day    keyword: day   default: 0 type: (and exact-integer? (not negative?)))
  (duration-date-time   keyword: time  default: 0 type: (and exact-integer? (not negative?))))

;;; ----------------------------------------

(define (duration->string dur)
  (string-append
   (case (duration-sign dur)
     ((+) "") ((-) "-"))
   (let ((d (duration-value dur)))
    (cond ((duration-date? d) (duration-date->string d))
          ;; ((duration-time? d) (duration-time->string d))
          ((duration-week? d) (duration-week->string d))
          (else (unreachable "duration-type serializer"))))))

(define-type (duration-full
              serializer:
              (lambda (r) `(string->duration ,(duration->string r))))
  (duration-sign type: (memv '(+ -))
                 keyword: sign)
  (duration-value type: (or duration-week? duration-date?)
                  keyword: value))



(define (duration-week->duration-date dur)
  (duration-date day: (* 7 (duration-week-week dur))))

(define (duration-ensure-date dur)
  (let ((d (duration-value dur)))
   (cond ((duration-date? d) d)
         ((duration-week? d) (duration-week->duration-date d))
         ;; ((duration-time? dur) (duration-time->duration-date dur))
         (else (scm-error 'type-error "duration-ensure-date"
                          "Non duration given: ~s"
                          (list dur) #f)))))

;;; For these, we promote to a proper type, then apply the inner transformation
(define ((duration-year* dur) f)
  (modify (duration-ensure-date dur)
          duration-date-year* f))

(define ((duration-month* dur) f)
  (modify (duration-ensure-date dur)
          duration-date-month* f))

(define ((duration-day* dur) f)
  (modify (duration-ensure-date dur)
          duration-date-day* f))


(define (duration-time* dur)
  (cond ((duration-date? dur) duration-date-time*)
        ;; ((duration-time? dur) identity-lens)
        ((duration-week? dur)
         (lens-compose duration-date-time*
                       (lambda (dur) 'TODO)))))


(define ((duration-hour* dur) f)
  (modify dur duration-time*
          (lambda (t)
            (let ((h r (floor/ t 3600)))
              (+ (* 3600 (f h)) r)))))

(define ((duration-minute* dur) f)
  (modify dur duration-time*
          (lambda (t)
            (let* ((h r (floor/ t 3600))
                   (m s (floor/ r 60)))
              (+ (* 3600 h) (* 60 (f m)) s)))))

(define ((duration-second* dur) f)
  (modify dur duration-time*
          (lambda (t)
            (let* ((h r (floor/ t 3600))
                   (m s (floor/ r 60)))
              (+ (* 3600 h) (* 60 m) (f s))))))

;; (define ((duration-minute* dt) f)
;;   (modify dur (duration-get-time-focus%* dur duration-time-minute*) f))
;; (define ((duration-secord* dt) f)
;;   (modify dur (duration-get-time-focus%* dur duration-time-second*) f))


(define* (duration key:
                   (sign '+)
                   year month day
                   hour minute second
                   week)
  (duration-full
   sign: sign
   value: (cond ((and week (or year month day hour minute second))
                 (scm-error 'type-error "duration"
                            "#:week can't be mixed with other keys"
                            '() #f))
                (week (duration-week week: week))
                ((not (or year month day))
                 (duration-date time: (+ (* 3600 (or hour   0))
                                         (*   60 (or minute 0))
                                         (or second 0))))
                (else
                 (duration-date year:  (or year  0)
                                month: (or month 0)
                                day:   (or day   0)
                                time: (+ (* 3600 (or hour   0))
                                         (*   60 (or minute 0))
                                         (or second 0)))))))

(define (duration-component? x)
  (or (duration-date? x)
      ;; (duration-time? x)
      (duration-week? x)))

(define duration? duration-full?)

;; (define (duration+ xs ...) ...)
;; (define (duration- xs ...) ...)

(define period-date-time-rx
  (make-regexp
   "^P([0-9]+Y)?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+S)?)?$"))
(define period-week-rx
  (make-regexp "^P([0-9]+)W$"))
(define (string->duration str)
  (define (extract m i)
    (cond ((match:substring m i)
           => (lambda (s)
                (string->number (string-drop-right s 1))))
          (else 0)))

  (cond ((string=? str "P")
         (scm-error 'misc-error "string->duration"
                    "String not parsable as duration: ~s"
                    (list str) #f))
        ((regexp-exec period-date-time-rx str)
         => (lambda (m)
              (duration
               year:  (extract m 1)
               month: (extract m 2)
               day:   (extract m 3)
               hour:   (extract m 5)
               minute: (extract m 6)
               second: (extract m 7))))
        ((regexp-exec period-week-rx str)
         => (lambda (m)
              (duration week: (string->number (match:substring m 1)))))
        (else
         (scm-error 'misc-error "string->duration"
                    "String not parsable as duration: ~s"
                    (list str) #f))))

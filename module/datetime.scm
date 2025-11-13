(define-module (datetime)
  ;; To resolve colision with cadr-second from srfi-1
  :replace (second)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)

  :use-module ((hnh util)
               :select (
                        vector-last
                        ->
                        ->>
                        swap
                        label
                        span-upto
                        ))
  :use-module ((hnh util env) :select (with-locale1))

  :use-module (hnh util object)
  :use-module (hnh util lens)

  :use-module (ice-9 i18n)
  :use-module (ice-9 format)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :use-module (calp util config)

  :export (date
           date?
           year month day
           year* month* day*

           time
           time?
           hour minute second
           hour* minute* second*

           datetime
           datetime?
           ;; get-date
           ;; get-timezone
           datetime-date
           datetime-time
           date* time*
           tz tz*

           date-zero?
           time-zero?

           datetime->unix-time
           unix-time->datetime

           current-datetime
           current-date

           get-datetime
           as-date
           as-time
           as-datetime

           leap-year?
           days-in-month
           days-in-year

           start-of-month
           end-of-month
           start-of-year

           date-stream
           day-stream
           month-stream
           week-stream

           time-min
           time-max
           date-min
           date-max
           datetime-min
           datetime-max

           week-day
           week-1-start
           week-number
           date-starting-week
           week-day-name

           timespan-overlaps?
           find-first-week-day
           all-wday-in-month
           all-wday-in-year
           in-date-range?

           weekday-list
           start-of-week
           end-of-week
           month-days
           days-in-interval
           year-day

           time->decimal-hour
           datetime->decimal-hour

           time->seconds
           seconds->time

           date-range

           datetime->string
           datetime->http-date
           date->string
           time->string

           parse-month
           string->datetime
           string->time
           string->date
           string->date/-time
           parse-ics-date
           parse-ics-time
           parse-ics-datetime
           parse-iso-date
           parse-iso-time
           parse-iso-datetime

           parse-freeform-date

           date= date=?
           time= time=?
           datetime= datetime=?

           date< date<? date<= date<=?
           date> date>? date>= date>=?
           time< time<? time<= time<=?
           time> time>? time>= time>=?
           datetime< datetime<? datetime<= datetime<=?
           datetime> datetime>? datetime>= datetime>=?
           date/-time< date/-time<? date/-time<= date/-time<=?
           date/-time> date/-time>? date/-time>= date/-time>=?

           date+ date-
           time+ time-
           datetime+ datetime-
           date-difference
           datetime-difference
           )
  :re-export (locale-month locale-month-short))


;;; Enums

(define-public jan  1) (define-public january 1)
(define-public feb  2) (define-public february 2)
(define-public mar  3) (define-public mars 3)
(define-public apr  4) (define-public april 4)
(define-public may  5)
(define-public jun  6) (define-public june 6)
(define-public jul  7) (define-public july 7)
(define-public aug  8) (define-public august 8)
(define-public sep  9) (define-public september 9)
(define-public oct 10) (define-public october 10)
(define-public nov 11) (define-public november 11)
(define-public dec 12) (define-public december 12)


(define-public sun 0) (define-public sunday 0)
(define-public mon 1) (define-public monday 1)
(define-public tue 2) (define-public tuesday 2)
(define-public wed 3) (define-public wednesday 3)
(define-public thu 4) (define-public thursday 4)
(define-public fri 5) (define-public friday 5)
(define-public sat 6) (define-public saturday 6)


;;; Configuration

;; (define-public week-start (make-parameter sun))
(define-config week-start sun
  description: "First day of week"
  pre: (ensure (lambda (x) (<= sun x sat))))



;;; RECORD TYPES

(define-type (date printer: (lambda (r p) (display (date->string r "#~1") p)))
  (year  default: 0 type: integer?)
  (month default: 0 type: integer?)
  (day   default: 0 type: integer?))

(define-type (time printer: (lambda (r p) (display (time->string r "#~3") p)))
  (hour   default: 0 type: integer?)
  (minute default: 0 type: integer?)
  (second default: 0 type: integer?))

(define (datetime-constructor-constructor constructor validator)
  (let ((date% date)
        (time% time))
   (lambda* (key: date time tz
                  (year 0) (month 0) (day 0)
                  (hour 0) (minute 0) (second 0))
     (let ((date (or date (date% year: year month: month day: day)))
           (time (or time (time% hour: hour minute: minute second: second))))
       (validator date time tz)
       (constructor date time tz)))))

(define-type (datetime
              constructor: datetime-constructor-constructor
              serializer: (lambda (r) `(datetime date: ,(datetime-date r) time: ,(datetime-time r) tz: ,(serialize (tz r))))
              printer: (lambda (r p)
                         (if (and (tz r) (not (string=? "UTC" (tz r))))
                             (write (datetime->sexp r) p) ; NOCOV
                             (display (datetime->string r "#~1T~3~Z") p))))

  (datetime-date type: date? lens: date*)
  (datetime-time type: time? lens: time*)
  ;; TODO actually implement tz.
  ;; It should either by an integer specifying second offset to the
  ;; epoch (so CEST would be 7200), or a string specifying the
  ;; timezone, either like "CEST" or "Europe/Stockholm".
  tz)


(define (datetime->http-date dt)
  (with-locale1
   LC_TIME "C"
   (lambda ()
     ;; TODO move dt to UTC
     (datetime->string dt "~a, ~d ~b ~Y ~H:~M:~S GMT"))))

(define (date-zero? date)
  (= 0 (year date) (month date) (day date)))

(define (time-zero? time)
  (= 0 (hour time) (minute time) (second time)))



;; NOTE there isn't any stable way to craft the tm objects.
;; I could call mktime on some date, and replace the fields
;; with the set-tm:*, but that is worse that breaking the API.
(define (datetime->tm datetime)
  (let ((t (datetime-time datetime))
        (d (datetime-date datetime)))
    (vector (second t)
            (minute t)
            (hour t)
            (day d)
            (1- (month d))
            (- (year d) 1900)
            0 0                ; wday & yday (ignored)
            -1                 ; DST unknown
            0                  ; UTC offset (ignored)
            (tz datetime)      ; TZ name
            )))

(define (tm->datetime tm)
  (datetime year:   (+ 1900 (tm:year tm))
            month:  (1+ (tm:mon  tm))
            day:    (tm:mday tm)
            hour:   (tm:hour tm)
            minute: (tm:min  tm)
            second: (tm:sec  tm)
            tz:     (tm:zone tm)))


(define (datetime->unix-time dt)
  (let ((tm (datetime->tm dt)))
    (car (if (tz dt)
             (mktime tm (vector-last tm))
             (mktime tm))))) ; NOCOV Would depend on local timezone

(define (unix-time->datetime n)
  ;; tm->datetime returns GMT here (as hinted by the
  ;; name @var{gmtime}). Blindly change it to UTC.
  (-> (tm->datetime (gmtime n))
      (tz "UTC")))


;; this returns UTC time, with a TZ component set to "UTC"
(define (current-datetime)
  (unix-time->datetime ((@ (guile) current-time))))

(define (current-date)
  (datetime-date (current-datetime)))




;; datetime → datetime
;; Takes a datetime in any timezone, and renormalize it to local time
;; (as defined by the environment variable TZ).
;; This means that given UTC 10:00 new years day
;; would return 11:00 new years day if ran in sweden.
(define (get-datetime dt)
  (let ((v (datetime->tm dt)))
    (let ((tm
           (localtime ; localtime convertion since the returned tm object is
            (car      ; in the parsed timezone.
             (cond [(not (tz dt)) (mktime v)]
                   [(string=? "local" (tz dt)) (mktime v)]
                   [else (mktime v (tz dt))])))))
      ;; strip tz-name, to conform with my local time.
      (-> (tm->datetime tm)
          (tz #f)))))

(define (as-date date/-time)
  (cond [date/-time datetime? => datetime-date]
        [(date? date/-time) date/-time]
        [(time? date/-time) (date)]
        [else (scm-error 'wrong-type-arg
                     "as-date"
                     "Object not a date, time, or datetime object ~a"
                     (list date/-time)
                     #f)]))

(define (as-time date/-time)
  (cond [date/-time datetime? => datetime-time]
        [(date? date/-time) (time)]
        [(time? date/-time) date/-time]
        [else (scm-error 'wrong-type-arg "as-time"
                     "Object not a date, time, or datetime object ~a"
                     (list date/-time)
                     #f)]))

(define (as-datetime dt)
  (cond [(datetime? dt) dt]
        [(date? dt) (datetime date: dt time: (time))]
        [(time? dt) (datetime time: dt date: (date))]
        [else (scm-error 'wrong-type-arg "as-datetime"
                     "Object not a date, time, or datetime object ~a"
                     (list dt)
                     #f)]))



;; int -> bool
(define (leap-year? year)
  (and (zero? (remainder year 4))
       (or (zero? (remainder year 400))
           (not (zero? (remainder year 100))))))

;; Returns number of days month for a given date. Just looks at the year and month components.
(define-public (days-in-month date)
  (define m (month date))
  (cond ((memv m (list jan mar may jul aug oct dec)) 31)
        ((memv m (list apr jun sep nov)) 30)
        ((and (= m feb) (leap-year? (year date))) 29)
        ((= m feb) 28)
        (else (scm-error 'out-of-range "days-in-month"
                         "No month number ~a (~a)"
                         (list (month date) date)
                         #f))))

(define (days-in-year date)
  (if (leap-year? (year date))
      366 365))

(define (start-of-month date)
  (-> date (day 1)))

(define (end-of-month date)
  (-> date (day (days-in-month date))))

(define (start-of-year date)
  (-> date
      (day 1)
      (month 1)))

(define (date-stream date-increment start-day)
  (stream-iterate (lambda (d) (date+ d date-increment))
                  start-day))

(define (day-stream start-day)
  (date-stream (date day: 1) start-day))

(define (month-stream start-day)
  (date-stream (date month: 1) start-day))

(define (week-stream start-day)
  (date-stream (date day: 7) start-day))

(define (time-min a b)
  (if (time<? a b) a b))

(define (time-max a b)
  (if (time<? a b) b a))

(define (date-min a b)
  (if (date< a b) a b))

(define (date-max a b)
  (if (date< a b) b a))

(define (datetime-min a b)
  (if (datetime< a b) a b))

(define (datetime-max a b)
  (if (datetime< a b) b a))

;; https://projecteuclid.org/euclid.acta/1485888738
;; 1. Begel.
;; J sei die Zahl des Jahrhunderts,
;; K die Jahrszahl innerhalb desselben,
;; m die Zahl des Monats,
;; q die Zahl des Monatstags,
;; h die Zahl des Wochentags;
(define (zeller J K m q)
  (modulo (+ q
             (floor-quotient (* 13 (1+ m))
                             5)
             K
             (floor-quotient K 4)
             5
             (- J))
          7))

;; 0 indexed, starting at sunday.
(define (week-day date)
  (let ((J K (floor/ (year date) 100))
        (m (month date)))
    (if (memv m '(1 2))
        (zeller J (1- K) (+ m 12) (day date))
        (zeller J K (month date) (day date)))))



;; given a date, returns the date the first week of that year starts on.
;; @example
;; (week-1-start #2020-01-01 mon)
;; ⇒ 2019-12-30
;; @end example
(define* (week-1-start d optional: (wkst (week-start)))
  (let* ((ystart (start-of-year d))
         (day-index (modulo (- (week-day ystart) wkst) 7)))
    (if (> day-index 3)
        (date+ ystart (date day: (- 7 day-index)))
        (date- ystart (date day: day-index)))))

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
                                   (date day: 3)))))
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
         (date day: (* (1- week-number) 7))))


(define* (week-day-name week-day-number optional: truncate-to
                        key: (locale %global-locale))

  ;; NOTE this allows days larger than 7 (sunday if counting from monday).
  (let ((str (locale-day (1+ (modulo week-day-number 7)) locale)))
    ;; I also know about the @var{locale-day-short} method, but I need
    ;; strings of length 2.
    (if truncate-to
        (string-take str truncate-to)
        str)))


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
  (or
   ;; A
   (and (date/-time<? s2-begin s1-end)
        (date/-time<? s1-begin s2-end))

   ;; B
   (and (date/-time<? s1-begin s2-end)
        (date/-time<? s2-begin s1-end))

   ;; C
   (and (date/-time<=? s1-begin s2-begin)
        (date/-time<? s2-end s1-end))

   ;; D
   (and (date/-time<=? s2-begin s1-begin)
        (date/-time<? s1-end s2-end))))


;; Returns the first instance of the given week-day after @var{d}.
;; @example
;; (find-first-week-day mon #2020-04-01)
;; => #2020-04-06
;; (find-first-week-day mon #2020-04-10)
;; => #2020-04-13
;; (find-first-week-day mon #2020-04-30)
;; => #2020-05-04
;; @end example
(define (find-first-week-day wday d)
  (let* ((start-day (week-day d))
         (diff (- wday start-day)))
    (date+ d (date day: (modulo diff 7)))))

;; returns instances of the given week-day in month between
;; month-date and end of month.
;; @example
;; (all-wday-in-month mon #2020-06-01)
;; => (#2020-06-01 #2020-06-08 #2020-06-15 #2020-06-22 #2020-06-29)
;; (all-wday-in-month mon #2020-06-10)
;; => (#2020-06-15 #2020-06-22 #2020-06-29)
;; @end example
;; week-day, date → (list date)
(define (all-wday-in-month wday month-date)
  (stream->list
   (stream-take-while
    (lambda (d) (= (month d) (month month-date)))
    (week-stream (find-first-week-day wday month-date)))))


(define (all-wday-in-year wday year-date)
  (stream->list
   (stream-take-while
    (lambda (d) (= (year d) (year year-date)))
    (week-stream (find-first-week-day wday year-date)))))


(define (in-date-range? start-date end-date)
  (lambda (date)
    (date<= start-date date end-date)))

;; Returns a list of the seven week days, with @var{week-start}
;; as the beginning of the week.
;; @example
;; (weekday-list sun)
;; => (0 1 2 3 4 5 6)
;; @end exampl
(define* (weekday-list optional: (week-start (week-start)))
  (take (drop (apply circular-list (iota 7))
              week-start)
        7))

;; returns the date the week containing d started.
;; (start-of-week #2020-04-02 sun) ; => 2020-03-29
(define* (start-of-week d optional: (week-start (week-start)))
  (date- d (date day: (modulo (- (week-day d)
                                 week-start)
                              7))))

;; (end-of-week #2020-04-01 mon)
;; => 2020-04-05
(define* (end-of-week d optional: (week-start (week-start)))
  (date+ (start-of-week d week-start)
         (date day: 6)))


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
         (prev-month-len (days-in-month (date- date* (date month: 1))))
         (month-start (modulo (- (week-day date*) week-start) 7)))
    (values
     (map (lambda (d) (-> date* (date- (date month: 1)) (day d)))
          (iota month-start (1+ (- prev-month-len month-start))))
     (map (lambda (d) (day date* d)) (iota month-len 1))
     (map (lambda (d) (-> date* (date+ (date month: 1)) (day d)))
          (iota (modulo (- (* 7 5) month-len month-start) 7) 1)))))


;; The amount of days in the given interval, both end pointts inclusive
(define (days-in-interval start-date end-date)
  (unless (date<= start-date end-date)
    (scm-error 'misc-error "days-in-interval"
               "End date must be greater (or equal) to start date: ~s, ~s"
               (list start-date end-date)
               #f))
  (let ((diff (date-difference (date+ end-date (date day: 1)) start-date)))
    (->> (month-stream start-date)
         (stream-take (+ (month diff)
                         (* 12 (year diff))))
         (stream-map days-in-month)
         (stream-fold + (day diff)))))


;; Day from start of the year, so 1 feb would be day 32.
;; Also known as Julian day.
(define (year-day date)
  (days-in-interval (start-of-year date) date))


;; @example
;; (time->decimal-hour #10:30:00) ; => 10.5
;; @end example
(define (time->decimal-hour time)
  (exact->inexact (+ (hour time)
                     (/ (minute time) 60)
                     (/ (second time) 3600))))

(define* (datetime->decimal-hour dt optional: start-date)

  (let ((date-diff
         (cond [start-date
                (let ((end-date (date+ start-date (datetime-date dt))))
                  (1- (days-in-interval start-date end-date)))]
               [(or (not (zero? (month (datetime-date dt))))
                    (not (zero? (year  (datetime-date dt)))))
                (scm-error 'misc-error "datetime->decimal-hour"
                       "Multi-month intervals only supported when start-date is given (~a)"
                       (list dt)
                       #f)]
               [else (-> dt datetime-date day)])))
    (-> dt datetime-time time->decimal-hour
        (+ (* date-diff 24)))))


(define (time->seconds t)
  (+ (* 60 60 (hour t))
     (* 60 (minute t))
     (second t)))

(define (seconds->time s)
  (when (negative? s)
    (scm-error 'misc-error "seconds->time"
               "Can't convert negative seconds to time values: ~s"
               (list s) #f))
  (let* ((hours minutes* (floor/ s (* 60 60)))
         (minutes seconds (floor/ minutes* 60)))
    (time hour: hours minute: minutes second: seconds)))


;; Returns a list of all dates from start to end.
;; both inclusive
;; date, date → [list date]
(define* (date-range start end optional: (increment (date day: 1)))
  (stream->list
   (stream-take-while (lambda (d) (date<= d end))
                      (date-stream increment start))))


;;; Output

(define* (datetime->string
          datetime
          optional:
          (fmt "~1T~3")
          (locale %global-locale)
          key: allow-unknown?)
  (define date (datetime-date datetime))
  (define time (datetime-time datetime))
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\H) (format #t "~2'0d" (hour time)))
                   ((#\k) (format #t "~2' d" (hour time)))
                   ((#\M) (format #t "~2'0d" (minute time)))
                   ((#\S) (format #t "~2'0d" (second time)))
                   ((#\Y) (format #t "~4'0d" (year date)))
                   ((#\m) (format #t "~2'0d" (month date)))
                   ((#\d) (format #t "~2'0d" (day date)))
                   ((#\e) (format #t "~2' d" (day date)))
                   ;; Should be same as ~_d
                   ((#\s) (display (datetime->unix-time datetime))) ; epoch time!
                   ((#\1) (display (datetime->string datetime "~Y-~m-~d")))
                   ((#\3) (display (datetime->string datetime "~H:~M:~S")))
                   ((#\A) (display (week-day-name (week-day date)   locale: locale)))
                   ((#\a) (display (week-day-name (week-day date) 3 locale: locale)))
                   ((#\B) (display (locale-month       (month date) locale)))
                   ((#\b) (display (locale-month-short (month date) locale)))
                   ((#\Z) (when (equal? "UTC" (tz datetime)) (display "Z")))
                   (else (unless allow-unknown?
                           (scm-error 'misc-error "datetime->string"
                                  "Invalid format token ~a"
                                  (list token)
                                  #f))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

(define* (date->string date optional: (fmt "~1") (locale %global-locale)
                       key: allow-unknown?)
  (datetime->string (datetime date: date)
                    fmt locale
                    allow-unknown?: allow-unknown?))

(define* (time->string time optional: (fmt "~3") (locale %global-locale)
                       key: allow-unknown?)
  (datetime->string (datetime time: time)
                    fmt locale
                    allow-unknown?: allow-unknown?))



;;; Input

(define* (parse-month str optional: (locale %global-locale))
  "Get month number from a (shortened) monthname.
Returns -1 on failure"
  (or
   (find (lambda (n)
           (define name (locale-month n locale))
           (define len (min (string-length name)
                            (string-length str)))
           (string-locale-ci=? (string-take str len)
                               (string-take name len)
                               locale))
         (iota 12 1))
   -1))

(define* (string->datetime string optional: (format-specifier "~Y-~m-~dT~H:~M:~S~Z")
                           (locale %global-locale)
                           key: return-trailing)

  (define (err fmt . args)
    (scm-error 'misc-error "string->datetime"
               (string-append "When parsing ~s as ~s; " fmt)
               (cons* string format-specifier args)
               (list string format-specifier)))

  (let loop* ((str (string->list string))
              (fmt (string->list format-specifier))
              (dt (datetime))
              (ampm identity))

    (define* (loop str fmt dt optional: (ampm ampm))
      (loop* str fmt dt ampm))

    (cond [(and (null? str) (null? fmt))
           (if return-trailing
               (values (ampm dt) '())
               (ampm dt))]
          [(null? str)
           ;; TODO it would be preferable to error out here. However, that fails for
           ;; optional specifiers (e.g. ~Z).
           ;; Also see the disabled test in "Premature end of string to parse"
           (if return-trailing
               (values (ampm dt) '())
               (ampm dt))
           #; (err "Premature end of string, trailing fmt: ~s" fmt)]
          [(null? fmt)
           (if return-trailing
               (values (ampm dt) str)
               (err "trailing characters: ~s" str))]
          [(and (eq? #\~ (car fmt))
                (null? (cdr fmt)))
           (err "Stray ~ at end of fmt")]
          [(eq? #\~ (car fmt))
           (case (cadr fmt)
             [(#\~) (if (eq? #\~ (car str))
                        (loop (cdr str)
                              (cddr fmt)
                              dt)
                        (err "mismatched symbol, expected ~s got ~s" #\~ (car str)))]
             [(#\Z)
              (if (eq? #\Z (car str))
                  (loop (cdr str)
                        (cddr fmt)
                        (tz dt "UTC"))
                  (loop str
                        (cddr fmt)
                        dt))]
             ;; AM/PM
             [(#\p)
              (cond ((string-match "^([AaPp])[.]?[Mm][.]?" (list->string str))
                     => (lambda (m)
                          (loop (drop str (match:end m))
                                (cddr fmt)
                                dt
                                (case (string-ref (match:substring m 1) 0)
                                  ((#\a #\A)
                                   (lambda (dt)
                                     (modify dt (lens-compose time* hour*)
                                             (lambda (x) (if (= x 12) 0 x)))))
                                  ((#\p #\P)
                                   (lambda (dt)
                                     (modify dt (lens-compose time* hour*)
                                             (lambda (x) (if (= x 12)
                                                        x (+ x 12))))))))
                          ))
                    ;; fail here?
                    (else (loop str (cddr fmt) dt)))
              ]
             ;; month by name
             [(#\b #\B #\h)
              (let ((head post
                          (match (cddr fmt)
                            (()                   (values str '()))
                            ;; Manual check so remaining cases becomes clearer
                            ((#\~)                (err "Unexpected ~ at end of fmt"))
                            ((#\~ #\~ rest ...)   (span (lambda (c) (not (eqv? #\~ c))) str))
                            ;; Dissalowed, since we otherwise have no idea where the month name ends.
                            ((#\~ rest ...)       (err "Can't have format specifier directly after month by name"))
                            ((next-char rest ...) (span (lambda (c) (not (eqv? c next-char))) str)))))
                (loop post
                      (cddr fmt)
                      (set dt (lens-compose date* month*)
                           (parse-month (list->string head) locale))))]
             [(#\H #\M #\S #\m #\d)
              ;; This captures both the possibility of a date with a single digit,
              ;; e.g. 7 may, but also compact, digits only, form without delimiters,
              ;; e.g. --0507,
              (let* ((pre post (span-upto 2 char-numeric? str))
                     (num (-> pre list->string string->number)))
                (loop
                 post
                 (cddr fmt)
                 (let ((lens
                        (case (cadr fmt)
                          [(#\H) (lens-compose time* hour*)]
                          [(#\M) (lens-compose time* minute*)]
                          [(#\S) (lens-compose time* second*)]
                          [(#\m) (lens-compose date* month*)]
                          [(#\d) (lens-compose date* day*)])))
                   (set dt lens num))))]

             [(#\Y)
              (let* ((pre post (span-upto 4 char-numeric? str))
                     (num (-> pre list->string string->number)))
                (loop
                 post
                 (cddr fmt)
                 (set dt (lens-compose date* year*) num)))]

             [else (err "Unimplemented or incorrect parse token ~S" str)])]
          [else
           (if (eq? (car str) (car fmt))
               (loop (cdr str)
                     (cdr fmt)
                     dt)
               (err "Mismatched symbol, expected ~s got ~s" (car fmt) (car str)))])))


;; TODO both string->time and string->date accepts format tokens which are invalid for them.
;; Should this be filtered out?

(define* (string->time str optional: (fmt "~H:~M:~S") (locale %global-locale)
                       key: return-trailing)
  (call-with-values
      (lambda () (string->datetime str fmt locale return-trailing: return-trailing))
    (case-lambda ((dt) (datetime-time dt))
                 ((dt rem) (values (datetime-time dt) rem)))))

(define* (string->date str optional: (fmt "~Y-~m-~d") (locale %global-locale)
                       key: return-trailing)
  (call-with-values
      (lambda () (string->datetime str fmt locale return-trailing: return-trailing))
    (case-lambda ((dt) (datetime-date dt))
                 ((dt rem) (values (datetime-time dt) rem)))))

;; Parse @var{string} as either a date, time, or date-time.
;; String MUST be on iso-8601 format.
(define (string->date/-time string)
  (define (contains symb)
    (lambda (string) (string-contains string symb)))

  (cond [string (contains "T") => string->datetime]
        [string (contains ":") => string->time]
        [string (contains "-") => string->date]
        [else (scm-error 'misc-error "string->date/-time"
                         "String doesn't look like a date, time or datetime: ~s"
                         (list string) (list string))]))


(define (parse-ics-date str)
  (string->date str "~Y~m~d"))

(define (parse-ics-time str)
  (string->time str "~H~M~S"))

(define* (parse-ics-datetime str optional: zone)
  (let ((dt (string->datetime str "~Y~m~dT~H~M~S~Z")))
    (if (tz dt)
        dt
        (tz dt zone))))

(define (parse-iso-date str)
  (string->date str))

(define (parse-iso-time str)
  (string->time str))

(define (parse-iso-datetime str)
  (string->datetime str))

(define (parse-freeform-date str)
  ;; TODO as-date?
  (parse-iso-datetime str))

(define (date->sexp d)
  `(date year: ,(year d)
         month: ,(month d)
         day: ,(day d)))

(define (time->sexp t)
  `(time hour: ,(hour t)
         minute: ,(minute t)
         second: ,(second t)))

(define* (datetime->sexp dt optional: verbose)
  `(datetime date: ,(if verbose (date->sexp (datetime-date dt)) (datetime-date dt))
             time: ,(if verbose (time->sexp (datetime-time dt)) (datetime-time dt))
             tz: ,(tz dt)))


(define (date-reader chr port)
  (define (dt->sexp dt) (datetime->sexp dt #t))
  (unread-char chr port)
  (let ((data (string->date/-time (symbol->string (read port)))))
    (cond [data datetime? => dt->sexp]
          [data time? => time->sexp]
          [data date? => date->sexp])))

(read-hash-extend #\0 date-reader)
(read-hash-extend #\1 date-reader)
(read-hash-extend #\2 date-reader)


;;; Everything below really messy

;;; EQUIALENCE

(define (date= . args)
  (reduce (lambda (a b)
            (and b ; did a previous iteration return false?
                 (= (year a) (year b))
                 (= (month a) (month b))
                 (= (day a) (day b))
                 ;; return object
                 a))
          #t args))

(define (time= . args)
  (reduce (lambda (a b)
            (and b
                 (= (hour a) (hour b))
                 (= (minute a) (minute b))
                 (= (second a) (second b))
                 a))
          #t args))

(define (datetime= . args)
  (reduce (lambda (a b)
            (and (date= (datetime-date a) (datetime-date b))
                 (time= (datetime-time a) (datetime-time b))
                 a))
          #t args))

(define date=? date=)
(define time=? time=)
(define datetime=? datetime=)


;; Extends a binary comparison procedure to work on any
;; number of arguments.
(define (fold-comparator <)
  (label this                           ; NOCOV
   (case-lambda
     [() #t]
     [(_) #t]
     [(first second . rest)
      (and (< first second)
           (apply this second rest))])))

(define date<
  (fold-comparator
   (lambda (a b)
     (let ((ay (year a))
           (by (year b)))
       (if (= ay by)
           (let ((am (month a))
                 (bm (month b)))
             (if (= am bm)
                 (< (day a) (day b))
                 (< am bm)))
           (< ay by))))))

(define date<=
  (fold-comparator
   (lambda (a b) (or (date= a b)
                (date< a b)))))

(define time<
  (fold-comparator
   (lambda (a b)
     (let ((ah (hour a))
           (bh (hour b)))
       (if (= ah bh)
           (let ((am (minute a))
                 (bm (minute b)))
             (if (= am bm)
                 (< (second a) (second b))
                 (< am bm)))
           (< ah bh))))))

(define time<=
  (fold-comparator
   (lambda (a b)
     (or (time= a b)
         (time< a b)))))

(define datetime<
  (fold-comparator
   (lambda (a b)
     (if (date= (datetime-date a) (datetime-date b))
         (time< (datetime-time a) (datetime-time b))
         (date< (datetime-date a) (datetime-date b))))))

(define datetime<=
  (fold-comparator
   (lambda (a b)
     (if (date= (datetime-date a) (datetime-date b))
         (time<= (datetime-time a) (datetime-time b))
         (date<= (datetime-date a) (datetime-date b))))))

(define date/-time<
  (fold-comparator
   (lambda (a b) (datetime< (as-datetime a) (as-datetime b)))))

(define date<?        date<)

(define date>         (swap date<))
(define date>?        (swap date<))

(define date<=?       date<=)

(define date>=        (swap date<=))
(define date>=?       (swap date<=))

(define time<?        time<)

(define time>         (swap time<))
(define time>?        (swap time<))

(define time<=?       time<=)

(define time>=        (swap time<=))
(define time>=?       (swap time<=))

(define datetime<?    datetime<)

(define datetime>     (swap datetime<))
(define datetime>?    (swap datetime<))

(define datetime<=?   datetime<=)

(define datetime>=    (swap datetime<=))
(define datetime>=?   (swap datetime<=))

(define date/-time<?  date/-time<)

(define date/-time>   (swap date/-time<))
(define date/-time>?  (swap date/-time<))

(define date/-time<=  (negate  date/-time>))
(define date/-time<=? (negate  date/-time>))

(define date/-time>=  (negate  date/-time<))
(define date/-time>=? (negate  date/-time<))




;;; OPERATIONS


;; NOTE +1 month is weird for late days in a month.
;; is the last of january +1 month the last of february,
;; or a few days into march? It's at least not the 31 of
;; February, as the code is currently written.
;; (date+ #2020-01-31 #0000-01-00) ; => 2020-02-31
(define (date+%% change base)

  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (days-in-month target) (+ (day change) (day target)))
          ;; No date overflow, just add the change
          (values (-> target (day (+ (day target)
                                     (day change))))
                  (day change 0))
          ;; Date (and possibly year) overflow
          (loop (if (= 12 (month target))
                    (-> (modify target year* 1+)
                        (month 1)
                        (day 1))
                    (-> (modify target month* 1+)
                        (day 1)))
                ;; How did this ever work‽
                (modify change day*
                        (lambda (d) (- d
                                  (- (day target))
                                  (days-in-month target)
                                  1)))))))

  (define-values (month-fixed change**)
    (if (date-zero? change*)
        (values days-fixed change*)
     (let loop ((target days-fixed) (change change*))
       (if (< 12 (+ (month change) (month target)))
           ;; if we overflow into the next year
           (loop (-> (modify target year* 1+)
                     (month 1))
                 (modify change month*
                         (lambda (d) (+ d (month target) -13))))
           ;; if we don't overflow our date
           (values (modify target month* (lambda (d) (+ d (month change))))
                   (month change 0))

           ))))

  ;; change** should here should have both month and date = 0

  (year month-fixed (+ (year month-fixed) (year change**))))

(define (date+% change base)

  (when (or (negative? (year change))
            (negative? (month change))
            (negative? (day change)))
    (scm-error 'misc-error "date+%" "Negative change ~a invalid (base=~a)"
               (list change base)
               #f))

  (unless (and (< 0 (month base))
               (< 0 (day base)))
    (scm-error 'misc-error "date+%"
           "~a needs day and month to be at least one"
           (list base)
           #f))

  (date+%% change base))

;; @var{base} MUST be a valid real date. all rest arguments can however
;; be "invalid" dates, such as 0000-00-10
(define (date+ base . rest)
  (fold date+% base rest))

(define (date-%% change base)
  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (day change) (day target))
          (let ((new-change (modify change day* (lambda (d) (- d (day target))))))
            (loop (if (= 1 (month target))
                      (-> (modify target year* 1-)
                          (month 12)
                          (day 31)              ; days in december
                          )
                      (let ((nm (modify target month* 1-)))
                        (day nm (days-in-month nm))))
                  new-change))
          (values (modify target day* (lambda (d) (- d (day change))))
                  (day change 0)))))

  (define-values (month-fixed change**)
    (let loop ((target days-fixed) (change change*))
      (if (>= (month change) (month target))
          (loop (-> (modify target year* 1-)
                    (month 12))
                (modify change month* (lambda (d) (- d (month target)))))
          (values (modify target month* (lambda (d) (- d (month change))))
                  (month change 0)))))

  ;; change** should here should have both month and date = 0

  (modify month-fixed year* (lambda (d) (- d (year change**)))))

(define (date-% change base)

  (when (or (negative? (year change))
            (negative? (month change))
            (negative? (day change)))
    (scm-error 'misc-error "date-%" "Negative change ~a invalid (base=~a)"
           (list change base)
           #f))

  (when (or (negative? (month base))
            (negative? (day base)))
    (scm-error 'misc-error "date-%"
           "~a needs day and month to be at least one"
           (list base)
           #f))

  (date-%% change base)
  )

;;; Only use this with extreme caution
(define (date- base . rest)
  (fold date-% base rest))

;;; time

;; overflow is number of days above
;; time x time → time x int
(define (time+% base change)

  ;; while (day base) > (days-in-month base)
  ;;     month++; days -= (days-in-month base)
  (define second-fixed
    (let loop ((target (modify base second* (lambda (d) (+ d (second change))))))
      (if (>= (second target) 60)
          (loop (-> target
                    (modify minute* 1+)
                    (modify second* (lambda (d) (- d 60)))))
          target)))

  ;; while (month base) > 12
  ;;     year++; month -= 12
  (define minute-fixed
    (let loop ((target (modify second-fixed minute* (lambda (d) (+ d (minute change))))))
      (if (>= (minute target) 60)
          (loop (-> target
                    (modify hour* 1+)
                    (modify minute* (lambda (d) (- d 60)))))
          target)))

  (define hour-almost-fixed (modify minute-fixed hour* (lambda (d) (+ d (hour change)))))

  (if (<= 24 (hour hour-almost-fixed))
      (let ((div remainder (floor/ (hour hour-almost-fixed) 24)))
        (values (hour hour-almost-fixed remainder) div))
      (values hour-almost-fixed 0)))

;;; PLUS
(define (time± proc)
  (lambda (base . rest)
   (let loop ((time-accumulated base) (overflow 0) (remaining rest))
     (if (null? remaining)
         (values time-accumulated overflow)
         (let ((next-time rem (proc time-accumulated (car remaining))))
           (loop next-time (+ overflow rem) (cdr remaining)))))))

(define time+ (time± time+%))

;; time, Δtime → time, hour
(define (time-% base change)

  (define-values (second-fixed change*)
    (let loop ((target base) (change change))
      (if (> (second change) (second target))
          (loop (-> (modify target minute* 1-)
                    (second 60))
                (modify change second* (lambda (d) (- d (second target)))))
          (values (modify target second* (lambda (d) (- d (second change))))
                  (second change 0)))))

  (define-values (minute-fixed change**)
    (let loop ((target second-fixed) (change change*))
      (if (> (minute change) (minute target))
          (loop (-> (modify target hour* 1-)
                    (minute 60))
                (modify change minute* (lambda (d) (- d (minute target)))))
          (values (modify target minute* (lambda (d) (- d (minute change))))
                  (minute change 0)))))

  (if (>= (hour minute-fixed) (hour change**))
      (values (modify minute-fixed hour* (lambda (d) (- d (hour change**)))) 0)
      (let ((diff (- (hour minute-fixed)
                     (hour change**))))
        (values (hour minute-fixed (modulo diff 24))
                (abs (floor (/ diff 24)))))))

;; Goes backwards from base, returning the two values:
;; the new time, and the number of days back we went.
;; Note that neither time+ or time- can return a time
;; component greater than 24h, but nothing is stoping
;; a user from creating them manually.
;; @lisp
;; (time- #10:00:00 #09:00:00) ; => 01:00:00 => 0
;; (time- #03:00:00 #07:00:00) ; => 20:00:00 => 1
;; (time- #10:00:00 (time hour: 48)) ; => 10:00:00 => 2
;; (time- #10:00:00 (time hour: (+ 48 4))) ; => 06:00:00 => 2
;; @end lisp
(define time- (time± time-%))


;;; DATETIME


(define (datetime+ base change)
  (let ((time* overflow (time+ (datetime-time base) (datetime-time change))))
    (-> base
        (modify date*
                (lambda (d) (date+ d
                              (datetime-date change)
                              (date day: overflow))))
        (datetime-time time*))))

(define (datetime- base change)
  (let ((time* underflow (time- (datetime-time base) (datetime-time change))))
    (-> base
        (modify date*
                (lambda (d) (date- d
                              (datetime-date change)
                              (date day: underflow))))
        (datetime-time time*))))

;;; the *-difference procedures takes two actual datetimes.
;;; date- instead takes a date and a delta (but NOT an actual date).

;; Works on 0-based dates. So the last of January 2020 becomes
;; 2020-00-30
(define (date-difference% b a)
  ;; #2020-01-01 #2020-00-26 → #2020-00-06 #2020-00-00
  (define-values (b* a*)
    (let loop ((b b) (a a))
      (if (> (day a) (day b))
          (let ((new-a (day a (- (day a) (day b) 1))))
            (loop (if (= 0 (month b))
                      (-> (modify b year* 1-)
                          (month 11)
                          (day 30)   ; Last day in december
                          )
                      (-> (modify b month* 1-)
                          (day (1- (days-in-month b))))) ; last in prev month
                  new-a))
          ;; elif (> (day b) (day a))
          (values (day b (- (day b) (day a)))
                  (day a 0)))))


  ;; (day a*) should be 0 here.

  (define-values (b** a**)
    (let loop ((b b*) (a a*))
      (if (> (month a) (month b))
          (loop (-> (modify b year* 1-)
                    (month 11))
                (modify a month* (lambda (d) (- d 1 (month b)))))
          ;; elif (> (month b) (month a))
          (values (modify b month* (lambda (d) (- d (month a))))
                  (month a 0)))))

  ;; a** should here should have both month and date = 0

  (year b** (- (year b**) (year a**))))



;; Earlier date after later date to have same semantics as subtraction
(define (date-difference later-date earlier-date)
  (when (date< later-date earlier-date)
    (scm-error 'misc-error "date-difference"
               "The earlier of the two dates must come after. later-date: ~a, earlier-date: ~a"
               (list later-date earlier-date) #f))
  (when (or (negative? (month later-date))
            (negative? (day   later-date))
            (negative? (month earlier-date))
            (negative? (day   earlier-date)) )
    (scm-error 'misc-error "date-difference"
           "~a or ~a contains negative months or days"
           (list earlier-date later-date)
           #f))

  (let ((proc (lambda (d) (-> d
                         (modify month* 1-)
                         (modify day* 1-)))))
    (date-difference% (proc later-date)
                      (proc earlier-date))))


;; NOTE, this is only properly defined when end is greater than start.
(define (datetime-difference end start)
  ;; NOTE Makes both start and end datetimes in the current local time.
  (let ((fixed-time overflow (time- (datetime-time end)
                                    (datetime-time start))))
    (datetime date: (date-difference (date- (datetime-date end)
                                            (date day: overflow))
                                     (datetime-date start))
              time: fixed-time
              ;; TODO TZ
              )))

(define-module (datetime)
  :export (date? year month day
                 hour minute second
                 time? datetime?)
  ;; To resolve colision with cadr-second from srfi-1
  :replace (second)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)

  :use-module (calp util)

  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (ice-9 i18n)
  :use-module (ice-9 format)
  :use-module (calp util config)
  :re-export (locale-month locale-month-short)
  )


;;; Enums

(define-many define-public
  (jan january   ) 1
  (feb february  ) 2
  (mar mars      ) 3
  (apr april     ) 4
  (may           ) 5
  (jun june      ) 6
  (jul july      ) 7
  (aug august    ) 8
  (sep september ) 9
  (oct october   ) 10
  (nov november  ) 11
  (dec december  ) 12)

(define-many define-public
  (sun sunday)    0
  (mon monday)    1
  (tue tuesday)   2
  (wed wednesday) 3
  (thu thursday)  4
  (fri friday)    5
  (sat saturday)  6)


;;; Configuration

(define-public week-start (make-parameter sun))
(define-config week-start sun
  description: "First day of week"
  pre: (ensure (lambda (x) (<= sun x sat)))
  post: week-start)


;;; RECORD TYPES

;;; DATE

(define-immutable-record-type <date>
  (make-date year month day)
  date?
  (year year) (month month) (day day))

(define*-public (date key: (year 0) (month 0) (day 0))
  (make-date year month day))

(set-record-type-printer!
 <date>
 (lambda (r p)
   (catch 'misc-error
     (lambda () (display (date->string r "#~Y-~m-~d") p))
     (lambda (err _ fmt args . rest)
       (format p "BAD~s-~s-~s" (year r) (month r) (day r))))))


;;; TIME

(define-immutable-record-type <time>
  (make-time hour minute second)
  time?
  (hour hour) (minute minute) (second second))

(define*-public (time key: (hour 0) (minute 0) (second 0))
  (make-time hour minute second))

(set-record-type-printer!
 <time>
 (lambda (r p)
   (catch 'misc-error
     (lambda () (display (time->string r "#~H:~M:~S") p))
     (lambda (err _ fmt args  rest)
       (format p "BAD~s:~s:~s"
               (hour r) (minute r) (second r))))))


;;; DATETIME

(define-immutable-record-type <datetime>
  (make-datetime date time tz)
  datetime?
  (date get-date)
  (time get-time%)
  (tz tz) ; #f for localtime, "UTC", "Europe/Stockholm", ...
  )

(export get-date)

(define-public (get-timezone datetime)
  (tz datetime))


(define*-public (datetime
                 key: date time
                 (year 0) (month 0) (day 0)
                 (hour 0) (minute 0) (second 0)
                 tz)
  (make-datetime (or date (make-date year month day))
                 (or time (make-time hour minute second))
                 tz))

(set-record-type-printer!
 <datetime>
 (lambda (r p)
   (if (and (tz r) (not (string=? "UTC" (tz r))))
       (write (datetime->sexp r) p)
       (display (datetime->string r "#~1T~3~Z") p))))




;; NOTE there isn't any stable way to craft the tm objects.
;; I could call mktime on some date, and replace the fields
;; with the set-tm:*, but that is worse that breaking the API.
(define (datetime->tm datetime)
  (let ((t (get-time% datetime))
        (d (get-date  datetime)))
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


(define-public (datetime->unix-time dt)
  (let ((tm (datetime->tm dt)))
    (car (if (tz dt)
             (mktime tm (vector-last tm))
             (mktime tm)))))

(define-public (unix-time->datetime n)
  ;; tm->datetime returns GMT here (as hinted by the
  ;; name @var{gmtime}). Blindly change it to UTC.
  (set (tz (tm->datetime (gmtime n)))
       "UTC"))


;; this returns UTC time, with a TZ component set to "UTC"
(define-public (current-datetime)
  (unix-time->datetime ((@ (guile) current-time))))

(define-public (current-date)
  (get-date (current-datetime)))




;; datetime → datetime
;; Takes a datetime in any timezone, and renormalize it to local time
;; (as defined by the environment variable TZ).
;; This means that given UTC 10:00 new years day
;; would return 11:00 new years day if ran in sweden.
(define-public (get-datetime dt)
  (let ((v (datetime->tm dt)))
    (let ((tm
           (localtime ; localtime convertion since the returned tm object is
            (car      ; in the parsed timezone.
             (cond [(not (tz dt)) (mktime v)]
                   [(string=? "local" (tz dt)) (mktime v)]
                   [else (mktime v (tz dt))])))))
      ;; strip tz-name, to conform with my local time.
      (set (tz (tm->datetime tm)) #f))))

(define-public (as-date date/-time)
  (cond [(datetime? date/-time) (get-date date/-time)]
        [(date? date/-time) date/-time]
        [(time? date/-time) (date)]
        [else (scm-error 'wrong-type-arg
                     "as-date"
                     "Object not a date, time, or datetime object ~a"
                     (list date/-time)
                     #f)]))

(define-public (as-time date/-time)
  (cond [(datetime? date/-time) (get-time% date/-time)]
        [(date? date/-time) (time)]
        [(time? date/-time) date/-time]
        [else (scm-error 'wrong-type-arg "as-time"
                     "Object not a date, time, or datetime object ~a"
                     (list date/-time)
                     #f)]))

(define-public (as-datetime dt)
  (cond [(datetime? dt) dt]
        [(date? dt) (datetime date: dt time: (time))]
        [(time? dt) (datetime time: dt date: (date))]
        [else (scm-error 'wrong-type-arg "as-datetime"
                     "Object not a date, time, or datetime object ~a"
                     (list dt)
                     #f)]))



(define-public (date-zero? date)
  (= 0 (year date) (month date) (day date)))

(define-public (time-zero? time)
  (= 0 (hour time) (minute time) (second time)))

;; int -> bool
(define-public (leap-year? year)
  (and (zero? (remainder year 4))
       (or (zero? (remainder year 400))
           (not (zero? (remainder year 100))))))

;; Returns number of days month for a given date. Just looks at the year and month components.
(define-public (days-in-month date)
  (case* (month date)
         ((jan mar may jul aug oct dec) 31)
         ((apr jun sep nov) 30)
         ((feb)
          (if (leap-year? (year date))
              29 28))
         (else (scm-error 'out-of-range "days-in-month"
                      "No month number ~a (~a)"
                      (list (month date) date)
                      #f))))

(define-public (days-in-year date)
  (if (leap-year? (year date))
      366 365))

(define-public (start-of-month date)
  (set (day date) 1))

(define-public (end-of-month date)
  (set (day date) (days-in-month date)))

(define-public (start-of-year date)
  (set-> date
         (day 1)
         (month 1)))

(define-public (date-stream date-increment start-day)
  (stream-iterate (lambda (d) (date+ d date-increment))
                  start-day))

(define-public (day-stream start-day)
  (date-stream (date day: 1) start-day))

(define-public (month-stream start-day)
  (date-stream (date month: 1) start-day))

(define-public (week-stream start-day)
  (date-stream (date day: 7) start-day))

(define-public (time-min a b)
  (if (time<? a b) a b))

(define-public (time-max a b)
  (if (time<? a b) b a))

(define-public (date-min a b)
  (if (date< a b) a b))

(define-public (date-max a b)
  (if (date< a b) b a))

(define-public (datetime-min a b)
  (if (datetime< a b) a b))

(define-public (datetime-max a b)
  (if (datetime< a b) b a))

(define*-public (month+ date-object #:optional (change 1))
  (date+ date-object (date month: change)))

(define*-public (month- date-object #:optional (change 1))
  (date- date-object (date month: change)))

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
(define-public (week-day date)
  (let* ((J K (floor/ (year date) 100))
         (m (month date)))
    (if (memv m '(1 2))
        (zeller J (1- K) (+ m 12) (day date))
        (zeller J K (month date) (day date)))))



;; given a date, returns the date the first week of that year starts on.
;; @example
;; (week-1-start #2020-01-01 mon)
;; ⇒ 2019-12-30
;; @end example
(define*-public (week-1-start d optional: (wkst (week-start)))
  (let* ((ystart (start-of-year d))
         (day-index (modulo (- (week-day ystart) wkst) 7)))
    (if (> day-index 3)
        (date+ ystart (date day: (- 7 day-index)))
        (date- ystart (date day: day-index)))))

;; (week-number #2020-01-01 mon) ; => 1
;; (week-number #2019-12-31 mon) ; => 1
(define*-public (week-number d optional: (wkst (week-start)))
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

(define*-public (date-starting-week
                 week-number d
                 optional: (wkst (week-start)))
  (date+ (week-1-start d wkst)
         (date day: (* (1- week-number) 7))))


(define*-public (week-day-name week-day-number optional: truncate-to
                               key: (locale %global-locale))

  ;; NOTE this allows days larger than 7 (sunday if counting from monday).
  (let ((str (catch 'out-of-range
               (lambda () (locale-day (1+ (modulo week-day-number 7)) locale))
               (lambda (oor str num)
                 (scm-error 'out-of-range "week-day-name"
                        "~a == (~a % 7) + 1"
                        (list num week-day-number)
                        #f)))))
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
(define-public (timespan-overlaps? s1-begin s1-end s2-begin s2-end)
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
(define-public (find-first-week-day wday d)
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
(define-public (all-wday-in-month wday month-date)
  (stream->list
   (stream-take-while
    (lambda (d) (= (month d) (month month-date)))
    (week-stream (find-first-week-day wday month-date)))))


(define-public (all-wday-in-year wday year-date)
  (stream->list
   (stream-take-while
    (lambda (d) (= (year d) (year year-date)))
    (week-stream (find-first-week-day wday year-date)))))

(define-public (add-day d)
  (date+ d (date day: 1)))

(define-public (remove-day d)
  (date- d (date day: 1)))

(define-public (in-date-range? start-date end-date)
  (lambda (date)
    (date<= start-date date end-date)))

;; Returns a list of the seven week days, with @var{week-start}
;; as the beginning of the week.
;; @example
;; (weekday-list sun)
;; => (0 1 2 3 4 5 6)
;; @end exampl
(define*-public (weekday-list optional: (week-start (week-start)))
  (take (drop (apply circular-list (iota 7))
              week-start)
        7))

;; returns the date the week containing d started.
;; (start-of-week #2020-04-02 sun) ; => 2020-03-29
(define*-public (start-of-week d optional: (week-start (week-start)))
  (date- d (date day: (modulo (- (week-day d)
                                 week-start)
                              7))))

;; (end-of-week #2020-04-01 mon)
;; => 2020-04-05
(define*-public (end-of-week d optional: (week-start (week-start)))
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
(define*-public (month-days date optional: (week-start (week-start)))
  (let* ((month-len (days-in-month date))
         (prev-month-len (days-in-month (month- date)))
         (month-start (modulo (- (week-day date) week-start) 7)))
    (values
     (map (lambda (d) (set (day (month- date)) d))
          (iota month-start (1+ (- prev-month-len month-start))))
     (map (lambda (d) (set (day date) d)) (iota month-len 1))
     (map (lambda (d) (set (day (month+ date)) d))
          (iota (modulo (- (* 7 5) month-len month-start) 7) 1)))))


(define-public (days-in-interval start-date end-date)
  (let ((diff (date-difference (date+ end-date (date day: 1)) start-date)))
    (with-streams
     (fold + (day diff)
           (map days-in-month
                (take (+ (month diff)
                         (* 12 (year diff)))
                      (month-stream start-date)))))))

;; Day from start of the year, so 1 feb would be day 32.
;; Also known as Julian day.
(define-public (year-day date)
  (days-in-interval (start-of-year date) date))


;; @example
;; (time->decimal-hour #10:30:00) ; => 10.5
;; @end example
(define-public (time->decimal-hour time)
  (exact->inexact (+ (hour time)
                     (/ (minute time) 60)
                     (/ (second time) 3600))))

(define*-public (datetime->decimal-hour dt optional: start-date)

  (let ((date-diff
         (cond [start-date
                (let* ((end-date (date+ start-date (get-date dt))))
                  (days-in-interval start-date end-date)) ]
               [(or (not (zero? (month (get-date dt))))
                    (not (zero? (year (get-date dt)))))
                (scm-error 'misc-error "datetime->decimal-hour"
                       "Multi-month intervals only supported when start-date is given (~a)"
                       (list dt)
                       #f)]
               [else (day (get-date dt))])))
    (+ (time->decimal-hour (get-time% dt))
       (* (1- date-diff) 24))))

;; Returns a list of all dates from start to end.
;; both inclusive
;; date, date → [list date]
(define*-public (date-range start end optional: (increment (date day: 1)))
  (stream->list
   (stream-take-while (lambda (d) (date<= d end))
                      (date-stream increment start))))


;;; Output

(define*-public (datetime->string
                 datetime
                 optional: (fmt "~Y-~m-~dT~H:~M:~S")
                 key: allow-unknown?)
  (define date (get-date datetime))
  (define time (get-time% datetime))
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
                   ;; Should be same as ~_d
                   ((#\s) (display (datetime->unix-time datetime))) ; epoch time!
                   ((#\e) (format #t "~2' d" (day date)))
                   ((#\1) (format #t "~4'0d-~2'0d-~2'0d"
                                  (year date) (month date) (day date)))
                   ((#\3) (format #t "~2'0d:~2'0d:~2'0d"
                                  (hour time) (minute time) (second time)))
                   ((#\A) (display (week-day-name (week-day date))))
                   ((#\a) (display (week-day-name (week-day date) 3)))
                   ((#\b) (display (locale-month-short (month date))))
                   ((#\B) (display (locale-month (month date))))
                   ((#\Z) (when (equal? "UTC" (get-timezone datetime)) (display "Z")))
                   (else (unless allow-unknown?
                           (scm-error 'misc-error "datetime->string"
                                  "Invalid format token ~a"
                                  (list token)
                                  #f))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

(define*-public (date->string date
                              optional: (fmt "~Y-~m-~d")
                              key: allow-unknown?)
  (datetime->string (datetime date: date)
                    fmt allow-unknown?: allow-unknown?))

(define*-public (time->string time
                              optional: (fmt "~H:~M:~S")
                              key: allow-unknown?)
  (datetime->string (datetime time: time)
                    fmt allow-unknown?: allow-unknown?))



;;; Input

(define*-public (string->datetime str optional: (fmt "~Y-~m-~dT~H:~M:~S~Z"))
  (let loop ((str (string->list str))
             (fmt (string->list fmt))
             (dt (datetime)))

    (define time (get-time% dt))
    (define date (get-date dt))
    (define (as-dt dt)
      (cond [(date? dt) (datetime date: dt time: time)]
            [(time? dt) (datetime date: date time: dt)]
            [else dt]))

    (cond [(null? str)
           ;; ((@ (calp util exceptions) warning)
           ;;  "Premature end of string, still got fmt = ~s"
           ;;  fmt)
           dt]
          [(null? fmt)
           ;; ((@ (calp util exceptions) warning)
           ;;  "Unsparsed characters at end of string")
           dt]
          [(eq? #\~ (car fmt))
           (case (cadr fmt)
             [(#\~) (if (eq? #\~ (car str))
                        (loop (cdr str)
                              (cddr fmt)
                              dt)
                        ;; TODO name the loop-local variables something
                        ;; other than the top level, to allow better error
                        ;; messages.
                        (scm-error 'misc-error "string->datetime"
                               "Mismatched symbol, expected ~a got ~a"
                               (list #\~ (car str))
                               #f))]
             [(#\Z)
              (if (eq? #\Z (car str))
                  (loop (cdr str)
                        (cdr fmt)
                        (set (tz dt) "UTC"))
                  (loop str
                        (cdr fmt)
                        dt))]
             [(#\H #\M #\S #\m #\d)
              (let* ((pre post (split-at str 2))
                     (num (-> pre list->string string->number)))
                (loop
                 post
                 (cddr fmt)
                 (as-dt
                  (case (cadr fmt)
                    [(#\H) (set (hour time)   num)]
                    [(#\M) (set (minute time) num)]
                    [(#\S) (set (second time) num)]
                    [(#\m) (set (month date)  num)]
                    [(#\d) (set (day date)    num)]))))]

             [(#\Y)
              (let* ((pre post (split-at str 4))
                     (num (-> pre list->string string->number)))
                (loop
                 post
                 (cddr fmt)
                 (as-dt (set (year date) num))))]

             [else
              (scm-error 'misc-error "string->datetime"
                     "Unimplemented or incorrect parse token ~S"
                     (list str)
                     #f)])]
          [else
           (if (eq? (car str) (car fmt))
               (loop (cdr str)
                     (cdr fmt)
                     dt)
               (scm-error 'misc-error "string->datetime"
                      "Mismatched symbol, expected ~a got ~a"
                      (list (car fmt) (car str))
                      #f))])))

(define*-public (string->time str optional: (fmt "~H:~M:~S"))
  (get-time% (string->datetime str fmt)))

(define*-public (string->date str optional: (fmt "~Y-~m-~d"))
  (get-date (string->datetime str fmt)))

;; Parse @var{string} as either a date, time, or date-time.
;; String MUST be on iso-8601 format.
(define-public (string->date/-time string)
  (define (contains symb)
    (lambda (string) (string-contains string symb)))

  (cond [string (contains "T") => string->datetime]
        [string (contains ":") => string->time]
        [string (contains "-") => string->date]))


(define-public (parse-ics-date str)
  (string->date str "~Y~m~d"))

(define-public (parse-ics-time str)
  (string->time str "~H~M~S"))

(define*-public (parse-ics-datetime str optional: zone)
  (let ((dt (string->datetime str "~Y~m~dT~H~M~S~Z")))
    (if (tz dt)
        dt
        (set (tz dt) zone))))

(define-public (parse-iso-date str)
  (string->date str))

(define-public (parse-iso-time str)
  (string->time str))

(define-public (parse-iso-datetime str)
  (string->datetime str))

(define-public (parse-freeform-date str)
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
  `(datetime date: ,(if verbose (date->sexp (get-date dt)) (get-date dt))
             time: ,(if verbose (time->sexp (get-time% dt)) (get-time% dt))
             tz: ,(tz dt)))


(define (date-reader chr port)
  (define (dt->sexp dt) (datetime->sexp dt #t) )
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

(define-public (date= . args)
  (reduce (lambda (a b)
            (and b ; did a previous iteration return false?
                 (= (year a) (year b))
                 (= (month a) (month b))
                 (= (day a) (day b))
                 ;; return object
                 a))
          #t args))

(define-public (time= . args)
  (reduce (lambda (a b)
            (and b
                 (= (hour a) (hour b))
                 (= (minute a) (minute b))
                 (= (second a) (second b))
                 a))
          #t args))

(define-public (datetime= . args)
  (reduce (lambda (a b)
            (and (date= (get-date a) (get-date b))
                 (time= (get-time% a) (get-time% b))
                 a))
          #t args))

(define-many define-public
  (date=?) date=
  (time=?) time=
  (datetime=?) datetime=)

(define (date<% a b)
  (let ((ay (year a))
        (by (year b)))
    (if (= ay by)
        (let ((am (month a))
              (bm (month b)))
          (if (= am bm)
              (< (day a) (day b))
              (< am bm)))
        (< ay by))))

(define-public date<
  (case-lambda
    [() #t]
    [(_) #t]
    [(first second . rest)
     (and (date<% first second)
          (apply date< second rest))]))

(define (date<=% a b)
  (or (date= a b)
      (date< a b)))

(define-public date<=
  (case-lambda
    [() #t]
    [(_) #t]
    [(first second . rest)
     (and (date<=% first second)
          (apply date<= second rest))]))

(define-public (time< a b)
  (let ((ah (hour a))
        (bh (hour b)))
    (if (= ah bh)
        (let ((am (minute a))
              (bm (minute b)))
          (if (= am bm)
              (< (second a) (second b))
              (< am bm)))
        (< ah bh))))

(define-public (time<= a b)
  (or (time= a b)
      (time< a b)))

(define-public (datetime< a b)
  (if (date= (get-date a) (get-date b))
      (time< (get-time% a) (get-time% b))
      (date< (get-date a) (get-date b))))

(define-public (datetime<= a b)
  (if (date= (get-date a) (get-date b))
      (time<= (get-time% a) (get-time% b))
      (date<= (get-date a) (get-date b))))

(define-public (date/-time< a b)
  (datetime< (as-datetime a) (as-datetime b)))

(define-many define-public
  (date<?) date<
  (date> date>?) (swap date<)
  (date<=?) date<=
  (date>= date>=?) (swap date<=)

  (time<?) time<
  (time> time>?) (swap time<)
  (time<=?) time<=
  (time>= time>=?) (swap time<=)

  (datetime<?) datetime<
  (datetime> datetime>?) (swap datetime<)
  (datetime<=?) datetime<=
  (datetime>= datetime>=?) (swap datetime<=)

  (date/-time<?) date/-time<
  (date/-time> date/-time>?) (swap date/-time<)
  (date/-time<= date/-time<=?) (negate date/-time>)
  (date/-time>= date/-time>=?) (negate date/-time<)
  )



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
          (values (set-> target (day = (+ (day change))))
                  (set-> change (day 0)))
          ;; Date (and possibly year) overflow
          (loop (if (= 12 (month target))
                    (set-> target
                           (year = (+ 1))
                           (month 1)
                           (day 1))
                    (set-> target
                           (month = (+ 1))
                           (day 1)))
                (set-> change
                       (day = (- (1+ (- (days-in-month target) (day target))))))))))

  (define-values (month-fixed change**)
    (if (date-zero? change*)
        (values days-fixed change*)
     (let loop ((target days-fixed) (change change*))
       (if (< 12 (+ (month change) (month target)))
           ;; if we overflow into the next year
           (loop (set-> target
                        (year = (+ 1))
                        (month 1))
                 (set (month change) = (- (- 13 (month target)))))

           ;; if we don't overflow our date
           (values (set (month target) = (+ (month change)))
                   (set (month change) 0))

           ))))

  ;; change** should here should have both month and date = 0

  (set (year month-fixed) = (+ (year change**))))

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
(define-public (date+ base . rest)
  (fold date+% base rest))

(define (date-%% change base)
  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (day change) (day target))
          (let ((new-change (set (day change) = (- (day target)))))
            (loop (if (= 1 (month target))
                      (set-> target
                             (year = (- 1))
                             (month 12)
                             (day 31)              ; days in december
                             )
                      (set-> target
                             (month = (- 1))
                             (day (days-in-month (set (month target) = (- 1))))))
                  new-change))
          (values (set (day target) = (- (day change)))
                  (set (day change) 0)))))

  (define-values (month-fixed change**)
    (let loop ((target days-fixed) (change change*))
      (if (>= (month change) (month target))
          (loop (set-> target
                       (year = (- 1))
                       (month 12))
                (set (month change) = (- (month target))))
          (values (set (month target) = (- (month change)))
                  (set (month change) 0)))))

  ;; change** should here should have both month and date = 0

  (set (year month-fixed) = (- (year change**))))

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
(define-public (date- base . rest)
  (fold date-% base rest))

;;; time

;; overflow is number of days above
;; time x time → time x int
(define-public (time+% base change)

  ;; while (day base) > (days-in-month base)
  ;;     month++; days -= (days-in-month base)
  (define second-fixed
    (let loop ((target (set (second base) = (+ (second change)))))
      (if (>= (second target) 60)
          (loop (set-> target
                       (minute = (+ 1))
                       (second = (- 60))))
          target)))

  ;; while (month base) > 12
  ;;     year++; month -= 12
  (define minute-fixed
    (let loop ((target (set (minute second-fixed) = (+ (minute change)))))
      (if (>= (minute target) 60)
          (loop (set-> target
                       (hour = (+ 1))
                       (minute = (- 60))))
          target)))

  (define hour-almost-fixed (set (hour minute-fixed) = (+ (hour change))))

  (if (<= 24 (hour hour-almost-fixed))
      (let* ((div remainder (floor/ (hour hour-almost-fixed) 24)))
        (values (set (hour hour-almost-fixed) remainder) div))
      (values hour-almost-fixed 0)))

;;; PLUS
(define-public (time+ base . rest)
  (let ((sum 0))
    (let ((time (fold (lambda (next done)
                        (let* ((next-time rem (time+% done next)))
                          (set! sum = (+ rem))
                          next-time))
                      base rest)))
      (values time sum))))

;; time, Δtime → time, hour
(define (time-% base change)

  (define-values (second-fixed change*)
    (let loop ((target base) (change change))
      (if (> (second change) (second target))
          (loop (set-> target
                       (minute = (- 1))
                       (second 60))
                (set (second change) = (- (second target))))
          (values (set (second target) = (- (second change)))
                  (set (second change) 0)))))

  (define-values (minute-fixed change**)
    (let loop ((target second-fixed) (change change*))
      (if (> (minute change) (minute target))
          (loop (set-> target
                       (hour = (- 1))
                       (minute 60))
                (set (minute change) = (- (minute target))))
          (values (set (minute target) = (- (minute change)))
                  (set (minute change) 0)))))

  (if (>= (hour minute-fixed) (hour change**))
      (values (set (hour minute-fixed) = (- (hour change**))) 0)
      (let ((diff (- (hour minute-fixed)
                     (hour change**))))
        (values (set (hour minute-fixed) (modulo diff 24))
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
(define-public (time- base . rest)
  (let ((sum 0))
    (let ((time (fold (lambda (next done)
                        (let* ((next-time rem (time-% done next)))
                          (set! sum = (+ rem))
                          next-time))
                      base rest)))
      (values time sum))))


;;; DATETIME


(define-public (datetime+ base change)
  (let* ((time overflow (time+ (get-time% base) (get-time% change))))
    (datetime date: (date+ (get-date base)
                           (get-date change)
                           (date day: overflow))
              time: time
              tz: (get-timezone base)
              )))

(define-public (datetime- base change)
  (let* ((time underflow (time- (get-time% base) (get-time% change))))
    (datetime date: (date- (get-date base)
                           (get-date change)
                           (date day: underflow))
              time: time
              tz: (tz base))))

;;; the *-difference procedures takes two actual datetimes.
;;; date- instead takes a date and a delta (but NOT an actual date).

;; Works on 0-based dates. So the last of January 2020 becomes
;; 2020-00-30
(define (date-difference% b a)
  ;; #2020-01-01 #2020-00-26 → #2020-00-06 #2020-00-00
  (define-values (b* a*)
    (let loop ((b b) (a a))
      (if (> (day a) (day b))
          (let ((new-a (set (day a) = (- (1+ (day b))))))
            (loop (if (= 0 (month b))
                      (set-> b
                             (year = (- 1))
                             (month 11)
                             (day 30)   ; Last day in december
                             )
                      (set-> b
                             (month = (- 1))
                             (day (1- (days-in-month b))))) ; last in prev month
                  new-a))
          ;; elif (> (day b) (day a))
          (values (set (day b) = (- (day a)))
                  (set (day a) 0)))))


  ;; (day a*) should be 0 here.

  (define-values (b** a**)
    (let loop ((b b*) (a a*))
      (if (> (month a) (month b))
          (loop (set-> b
                       (year = (- 1))
                       (month 11))
                (set (month a) = (- (1+ (month b)))))
          ;; elif (> (month b) (month a))
          (values (set (month b) = (- (month a)))
                  (set (month a) 0)))))

  ;; a** should here should have both month and date = 0

  (set (year b**) = (- (year a**))))



;; NOTE, this is only properly defined when b is greater than a.
(define-public (date-difference b a)
  (when (or (negative? (month b))
            (negative? (day   b))
            (negative? (month a))
            (negative? (day   a)) )
    (scm-error 'misc-error "date-difference"
           "~a or ~a contains negative months or days"
           (list a b)
           #f))

  (date-difference% (set-> b
                           (month = (- 1))
                           (day = (- 1)))
                    (set-> a
                           (month = (- 1))
                           (day = (- 1)))))


;; NOTE, this is only properly defined when end is greater than start.
(define-public (datetime-difference end start)
  ;; NOTE Makes both start and end datetimes in the current local time.
  (let* ((fixed-time overflow (time- (get-time% end)
                                     (get-time% start))))
    (datetime date: (date-difference (date- (get-date end)
                                            (date day: overflow))
                                     (get-date start))
              time: fixed-time)))

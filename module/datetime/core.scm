;;; Commentary:
;;; IMPORTANT: This module MUST NOT import any other part of the
;;; (datetime) tree, in order to avoid cycles in the dependency graph.
;;; Code:
(define-module (datetime core)
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
                        when unless
                        ))
  :use-module (hnh util type)

  :use-module (hnh util object)
  :use-module (hnh util serialize)
  :use-module (hnh util lens)

  :use-module (ice-9 format)
  :use-module (ice-9 regex)
  :use-module (ice-9 curried-definitions)

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
           datetime-date date*
           datetime-time time*
           tz tz*

           date-zero?
           time-zero?

           utc-datetime?
           zoned-datetime?
           unzoned-datetime?

           datetime->unix-time
           unix-time->datetime

           current-datetime
           current-date

           leap-year?
           days-in-month
           days-in-year

           start-of-month
           end-of-month

           time-min
           time-max
           date-min
           date-max
           datetime-min
           datetime-max

           week-day


           weekday-list

           time->decimal-hour

           time->seconds
           seconds->time


           date= date=?
           time= time=?
           datetime= datetime=?

           date< date<? date<= date<=?
           date> date>? date>= date>=?
           time< time<? time<= time<=?
           time> time>? time>= time>=?
           datetime< datetime<? datetime<= datetime<=?
           datetime> datetime>? datetime>= datetime>=?
           )
  )


;;; Enums

(define-public jan  1) (define-public january   jan)
(define-public feb  2) (define-public february  feb)
(define-public mar  3) (define-public mars      mar)
(define-public apr  4) (define-public april     apr)
(define-public may  5)
(define-public jun  6) (define-public june      jun)
(define-public jul  7) (define-public july      jul)
(define-public aug  8) (define-public august    aug)
(define-public sep  9) (define-public september sep)
(define-public oct 10) (define-public october   oct)
(define-public nov 11) (define-public november  nov)
(define-public dec 12) (define-public december  dec)


(define-public sun 0) (define-public sunday    sun)
(define-public mon 1) (define-public monday    mon)
(define-public tue 2) (define-public tuesday   tue)
(define-public wed 3) (define-public wednesday wed)
(define-public thu 4) (define-public thursday  thu)
(define-public fri 5) (define-public friday    fri)
(define-public sat 6) (define-public saturday  sat)


;;; Configuration

(define-public week-start
  (make-parameter
   sun
   (lambda (val)
     (unless (and (exact-integer? val)
                  (<= sun val sat))
       (error "Invalid week start" val))
     val)))



;;; RECORD TYPES

(define-type (date printer: (lambda (r p) (format p "#~a" (date->string/simple r))))
  (year  default: 0 type: integer?)
  (month default: 0 type: integer?)
  (day   default: 0 type: integer?))

(define-type (time printer: (lambda (r p) (format p "#~a" (time->string/simple r))))
  (hour   default: 0 type: integer?)
  (minute default: 0 type: integer?)
  (second default: 0 type: integer?))

(define (datetime-constructor-constructor constructor validator)
  (let ((date% date)
        (time% time))
    (lambda* (key: date time tz
                   (year 0) (month 0) (day 0)
                   (hour 0) (minute 0) (second 0)
                   rest: rest)
      (let ((date (or date (date% year: year month: month day: day)))
            (time (or time (time% hour: hour minute: minute second: second))))
        (validator date time tz)
        (constructor date time tz)))))

(define (datetime-serializer dt)
  ;; record->list NOT used, since we look at parts of the fields
  ;; directly
  (let ((d (datetime-date dt))
        (t (datetime-time dt)))
    `(datetime
      ,@(unless (zero? (year d))   `(year:   ,(year d)))
      ,@(unless (zero? (month d))  `(month:  ,(month d)))
      ,@(unless (zero? (day d))    `(day:    ,(day d)))
      ,@(unless (zero? (hour t))   `(hour:   ,(hour t)))
      ,@(unless (zero? (minute t)) `(minute: ,(minute t)))
      ,@(unless (zero? (second t)) `(second: ,(second t)))
      ,@(when (tz dt) `(tz: ,(tz dt))))))

(define-type (datetime
              constructor: datetime-constructor-constructor
              serializer: datetime-serializer
              printer: (lambda (r p)
                         (cond ((not (tz r))
                                (format p "#~a" (datetime->string/simple r)))
                               ((string=? "UTC" (tz r))
                                (format p "#~aZ" (datetime->string/simple r)))
                               (else
                                (format p "#.(tz #~a ~s)"
                                        (datetime->string/simple r)
                                        (tz r))))))

  (datetime-date type: date? lens: date*)
  (datetime-time type: time? lens: time*)
  ;; TODO extend this type, to be one of
  ;; - false?: "local time", or a datetime offset
  ;; - string?: a reference to the installed zoneinfo database
  ;; - (eq? 'UTC): UTC time, instead of the current where the string
  ;;               "UTC" gets special treatment.
  ;; - timespec?: exact UTC offset, instead of the current overloading
  ;;              of strings on the form "UTC+\d*"
  ;; - some representation of timezones from calendar streams:
  ;;   iCalendar streams carry along their own zoneinfo, which
  ;;   completely ignore any other database. These rules MUST be copied
  ;;   into each relevant datetime object.
  (tz type: (or false? string?)))


(define (date-zero? date)
  (= 0 (year date) (month date) (day date)))

(define (time-zero? time)
  (= 0 (hour time) (minute time) (second time)))

(define (utc-datetime? x)
  (and (datetime? x)
       (equal? "UTC" (tz x))))

(define (zoned-datetime? x)
  (and (datetime? x)
       ;; "UTC" is also a zone
       (string? (tz x))))

(define (unzoned-datetime? x)
  (and (datetime? x)
       (not (tz x))))



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








;; @example
;; (time->decimal-hour #10:30:00) ; => 10.5
;; @end example
(define (time->decimal-hour time)
  (exact->inexact (+ (hour time)
                     (/ (minute time) 60)
                     (/ (second time) 3600))))


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





;;; EQUVIALENCE

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
  (unless (apply equal? (map tz args))
    (scm-error
     'wrong-type-arg "datetime="
     "Datetime equivalence only defined for matching timezones. Got: ~s"
     (list args) #f))

  (reduce (lambda (a b)
            (and b
                 (date= (datetime-date a) (datetime-date b))
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
     (typecheck a (or utc-datetime? unzoned-datetime?) "datetime<")
     (typecheck b (or utc-datetime? unzoned-datetime?) "datetime<")
     (unless (equal? (tz a) (tz b))
       (scm-error 'wrong-type-arg "datetime<"
                  "All datetimes must be UTC or zoneless. Got: ~s & ~s"
                  (list a b) #f))
     (if (date= (datetime-date a) (datetime-date b))
         (time< (datetime-time a) (datetime-time b))
         (date< (datetime-date a) (datetime-date b))))))

(define datetime<=
  (fold-comparator
   (lambda (a b)
     (or (datetime= a b)
         (datetime< a b)))))



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




(define (time->string/simple time)
  (format #f "~2'0d:~2'0d:~2'0d"
          (hour time)
          (minute time)
          (second time)))

(define (date->string/simple date)
  (format #f "~4'0d-~2'0d-~2'0d"
          (year date)
          (month date)
          (day date)))

(define (datetime->string/simple datetime)
  (string-append (date->string/simple (datetime-date datetime))
                 "T"
                 (time->string/simple (datetime-time datetime))))

(define time-pat "([0-9]{2}):([0-9]{2})(:([0-9]{2}))?")
(define date-pat "([0-9]{4,})-([0-9]{2})-([0-9]{2})")
(define time-rx (make-regexp (format #f "^~a$" time-pat)))
(define date-rx (make-regexp (format #f "^~a$" date-pat)))
(define datetime-rx (make-regexp (format #f "^~aT~a(Z)?$" date-pat time-pat)))

;; Parse @var{string} as either a date, time, or date-time.
;; String MUST be on iso-8601 format.
(define (string->date/-time string)
  (cond ((regexp-exec datetime-rx string)
         => (lambda (m)
              (datetime
               year:   (string->number (match:substring m 1))
               month:  (string->number (match:substring m 2))
               day:    (string->number (match:substring m 3))
               hour:   (string->number (match:substring m 4))
               minute: (string->number (match:substring m 5))
               second: (cond ((match:substring m 7) => string->number)
                             (else 0))
               tz: (and (match:substring m 8) "UTC"))))

        ((regexp-exec date-rx string)
         => (lambda (m)
              (date
               year:   (string->number (match:substring m 1))
               month:  (string->number (match:substring m 2))
               day:    (string->number (match:substring m 3)))))

        ((regexp-exec time-rx string)
         => (lambda (m)
              (time
               hour:   (string->number (match:substring m 1))
               minute: (string->number (match:substring m 2))
               second: (cond ((match:substring m 4) => string->number)
                             (else 0)))))
        (else
         (scm-error 'misc-error "string->date/-time"
                    "String doesn't look like a date, time or datetime: ~s"
                    (list string) (list string)))))


(define (date-reader chr port)
  (unread-char chr port)
  (-> (read port)
      symbol->string
      string->date/-time
      serialize))

(read-hash-extend #\0 date-reader)
(read-hash-extend #\1 date-reader)
(read-hash-extend #\2 date-reader)

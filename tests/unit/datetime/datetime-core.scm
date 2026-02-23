(define-module (test datetime-core)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((srfi srfi-41)
               :select (stream->list stream-take))
  :use-module (datetime core)
  :use-module ((ice-9 i18n) :select (make-locale))
  :use-module ((guile) :select (LC_CTYPE LC_TIME)))


;;; Global locale objects, to save all tests from creating them
(define en_US (make-locale (list LC_CTYPE LC_TIME) "en_US.UTF-8"))
(define sv_SE (make-locale (list LC_CTYPE LC_TIME) "sv_SE.UTF-8"))

;;; These tests begin by testing the basic objects,
;;; followed by the string parser.
;;; This to finally test the read syntax, to be able to
;;; us it in the rest of the tests.

(test-group "Creation of basic objects"
  (test-group "Date"
    (test-group "Empty date"
      (let ((d (date)))
        (test-assert "Date creates date objects" (date? d))
        (test-equal "Year is zero"  0 (year d))
        (test-equal "Month is zero" 0 (month d))
        (test-equal "Day is zero"   0 (day d))
        (test-assert "Date-zero? agrees" (date-zero? d))))

    (test-group "Date with keys"
      ;; Implicitly tests that month and day can go above
      ;; "regular" bounds
      (let ((d (date day: 40 month: 20 year: 10)))
        (test-assert "Date creation still works" (date? d))
        (test-equal "Year is stored"  10 (year d))
        (test-equal "Month is stored" 20 (month d))
        (test-equal "Day is stored"   40 (day d))))

    (test-group "Can't create date with non-integer components"
      (test-error "Invalid year"  'wrong-type-arg (date year: #f))
      (test-error "Invalid month" 'wrong-type-arg (date month: #f))
      (test-error "Invalid day"   'wrong-type-arg (date day: #f))))

  (test-group "Time"
    (test-group "Empty time"
      (let ((t (time)))
        (test-assert "Time creates time objects" (time? t))
        (test-equal "hour is zero"   0 (hour t))
        (test-equal "minute is zero" 0 (minute t))
        (test-equal "second is zero" 0 (second t))
        (test-assert "Time zero agrees" (time-zero? t))))

    (test-group "Time with keys"
      (let ((t (time second: 10 minute: 20 hour: 30)))
        (test-assert "Time creation still works" (time? t))
        (test-equal "Hour is stored"   30 (hour t))
        (test-equal "Minute is stored" 20 (minute t))
        (test-equal "Second is stored" 10 (second t))))

    (test-group "Can't create time with non-integer components"
      (test-error "Invalid hour"   'wrong-type-arg (time hour: #f))
      (test-error "Invalid minute" 'wrong-type-arg (time minute: #f))
      (test-error "Invalid second" 'wrong-type-arg (time second: #f))))

  (test-group "Datetime"
    (let ()
      (test-group "Empty datetime"
        (let ((dt (datetime)))
          (test-assert "Datetime date is date" (date? (datetime-date dt)))
          (test-assert "Datetime date is zero" (date-zero? (datetime-date dt)))
          (test-assert "Datetime time is time" (time? (datetime-time dt)))
          (test-assert "Datetime time is zero" (time-zero? (datetime-time dt)))
          (test-eqv "Defalut timezone is #f" #f (tz dt))))

      (test-group "Datetime with keys"
        (let ((dt (datetime date: (date day: 10)
                            time: (time minute: 20))))
          (test-equal "Given date is stored"
            10 (day (datetime-date dt)))
          (test-equal "Given time is stored"
            20 (minute (datetime-time dt))))
        (test-error "Date must be a date" 'wrong-type-arg (datetime date: 1))
        (test-error "Date must be a date" 'wrong-type-arg (datetime date: (time)))
        (test-assert "Date: #f gives still constructs a date" (date? (datetime-date (datetime date: #f))))
        (test-error "Time must be a time" 'wrong-type-arg (datetime time: 1))
        (test-error "Time must be a time" 'wrong-type-arg (datetime time: (date)))
        (test-assert "Time: #f gives still constructs a time" (time? (datetime-time (datetime time: #f))))

        (let ((dt (datetime hour: 20 day: 30)))
          (test-equal "Time objects can be implicitly created" 20 (hour (datetime-time dt)))
          (test-equal "Date objects can be implicitly created" 30 (day (datetime-date dt))))
        (let ((dt (datetime day: 30 time: (time hour: 20))))
          (test-equal "\"Upper\" and \"lower\" keys can be mixed"
            20 (hour (datetime-time dt)))
          (test-equal "\"Upper\" and \"lower\" keys can be mixed"
            30 (day (datetime-date dt))))

        (let ((dt (datetime hour: 30 time: (time hour: 20))))
          (test-equal "time: has priority over hour: (and the like)"
            20 (hour (datetime-time dt)))))
      (let ((dt (datetime day: 30 date: (date day: 20))))
        (test-equal "date: has priority over day: (and the like)"
          20 (day (datetime-date dt)))))))





#;
(test-group "Reader extensions"

  ;; All tests have a list variant, to ensure that it plays nice with the rest
  ;; of scheme's syntax

  (test-equal "Basic time read syntax"
    (time hour: 10 minute: 20 second: 30)
    (test-read-eval-string "#10:20:30"))

  (test-equal "Basic time read syntax in list"
    (list (time hour: 10 minute: 20 second: 30))
    (test-read-eval-string "(list #10:20:30)"))

  (test-equal "Basic date read syntax"
    (date year: 2020 month: 3 day: 10)
    (test-read-eval-string "#2020-03-10"))

  (test-equal "Basic date read syntax in list"
    (list (date year: 2020 month: 3 day: 10))
    (test-read-eval-string "(list #2020-03-10)"))

  (test-equal "Basic datetime read syntax"
    (datetime date: (date year: 2020 month: 3 day: 10)
              time: (time hour: 10 minute: 20 second: 30))
    (test-read-eval-string "#2020-03-10T10:20:30"))

  (test-equal "Basic datetime read syntax in list"
    (list (datetime date: (date year: 2020 month: 3 day: 10)
                    time: (time hour: 10 minute: 20 second: 30)))
    (test-read-eval-string "(list #2020-03-10T10:20:30)"))

  (test-equal "Basic datetime read syntax with Z"
    (datetime date: (date year: 2020 month: 3 day: 10)
              time: (time hour: 10 minute: 20 second: 30)
              tz: "UTC")
    (test-read-eval-string "#2020-03-10T10:20:30Z"))

  (test-equal "Basic datetime read syntax with Z in list"
    (list
     (datetime date: (date year: 2020 month: 3 day: 10)
               time: (time hour: 10 minute: 20 second: 30)
               tz: "UTC"))
    (test-read-eval-string "(list #2020-03-10T10:20:30Z)"))
  )




(test-equal "Datetime->unix-time"
  1656005146 (datetime->unix-time (datetime year: 2022 month: 06 day: 23 hour: 17 minute: 25 second: 46 tz: "UTC")))

(test-equal "Datetime->unix-time before epoch"
  -62167219200
  (datetime->unix-time (datetime year: 0000 month: 01 day: 01 hour: 00 minute: 00 second: 00 tz: "UTC")))

(test-equal "unix-time->datetime" (datetime year: 2020 month: 09 day: 13 hour: 12 minute: 26 second: 40 tz: "UTC")
            (unix-time->datetime 1600000000))
(test-equal "unix-time->datetime on epoch" (datetime year: 1970 month: 01 day: 01 hour: 00 minute: 00 second: 00 tz: "UTC")
            (unix-time->datetime 0))
(test-equal "unix-time->datetime before epoch" (datetime year: 1919 month: 04 day: 20 hour: 11 minute: 33 second: 20 tz: "UTC")
            (unix-time->datetime -1600000000))

;; (unix-time->datetime (expt 2 31)) ; => (datetime year: 2038 month: 01 day: 19 hour: 03 minute: 14 second: 08 tz: "UTC")
;; (unix-time->datetime (1+ (expt 2 31))) ; => (datetime year: 2038 month: 01 day: 19 hour: 03 minute: 14 second: 09 tz: "UTC")
;; (unix-time->datetime (- (expt 2 31))) ; => (datetime year: 1901 month: 12 day: 13 hour: 20 minute: 45 second: 52 tz: "UTC")


(test-assert "Current datetime returns a datetime"
  (datetime? (current-datetime)))
(test-equal "Current datetime returns with tz: UTC"
  "UTC" (tz (current-datetime)))
(test-assert "Current-date returns a date"
  (date? (current-date)))



(test-group "Leap years"
  (test-assert "Most years are't leap years" (not (leap-year? 1999)))
  (test-assert "Except if it's divisible by 4"    (leap-year? 2020))
  (test-assert "But not by 100"              (not (leap-year? 1900)))
  (test-assert "Except if also divisible by 400"  (leap-year? 2000)))

(test-assert "31 days in most month" (days-in-month (date month: jan)))
(test-assert "30 days in some month" (days-in-month (date month: apr)))
(test-assert "28 days in februrary on regular year"
  (days-in-month (date month: feb year: 2022)))
(test-assert "29 days in februrary on leap year"
  (days-in-month (date month: feb year: 2000)))
(test-error "To low month"  'out-of-range (days-in-month (date month: 0)))
(test-error "To high month" 'out-of-range (days-in-month (date month: 13)))

(test-equal "365 days in regular year" 365 (days-in-year (date year: 2022)))
(test-equal "366 days in leap year" 366 (days-in-year (date year: 2000)))

(test-equal "Start of month" (date year: 2020 month: 01 day: 01) (start-of-month (date year: 2020 month: 01 day: 15)))
(test-equal "End of month" (date year: 2000 month: 02 day: 29) (end-of-month (date year: 2000 month: 02 day: 01)))

(test-equal "Start of year" (date year: 2020 month: 01 day: 01) (start-of-year (date year: 2020 month: 12 day: 31)))
;; Note that end-of-year (apparently) doesn't exist

(test-group "Date streams"
  (test-equal "Day stream"
    (list (date year: 2020 month: 01 day: 01)
          (date year: 2020 month: 01 day: 02)
          (date year: 2020 month: 01 day: 03)
          (date year: 2020 month: 01 day: 04)
          (date year: 2020 month: 01 day: 05))
    (stream->list 5 (day-stream (date year: 2020 month: 01 day: 01))))

)

;; See time< tests for more context
(test-group "Min/max"
  (test-equal "Time min"
    (time hour: 07 minute: 40 second: 50) (time-min (time hour: 10 minute: 20 second: 30) (time hour: 07 minute: 40 second: 50)))
  (test-equal "Time max"
    (time hour: 10 minute: 20 second: 30) (time-max (time hour: 10 minute: 20 second: 30) (time hour: 07 minute: 40 second: 50)))

  (test-equal "Date min"
    (date year: 2020 month: 02 day: 02) (date-min (date year: 2020 month: 02 day: 02) (date year: 2020 month: 03 day: 01)))
  (test-equal "Date max"
    (date year: 2020 month: 03 day: 01) (date-max (date year: 2020 month: 02 day: 02) (date year: 2020 month: 03 day: 01)))

  (test-equal "Datetime min"
    (datetime year: 2020 month: 02 day: 02 hour: 10 minute: 20 second: 30) (datetime-min (datetime year: 2020 month: 02 day: 02 hour: 10 minute: 20 second: 30) (datetime year: 2020 month: 03 day: 01 hour: 07 minute: 40 second: 50)))
  (test-equal "Datetime max"
    (datetime year: 2020 month: 03 day: 01 hour: 07 minute: 40 second: 50) (datetime-max (datetime year: 2020 month: 02 day: 02 hour: 10 minute: 20 second: 30) (datetime year: 2020 month: 03 day: 01 hour: 07 minute: 40 second: 50))))

(test-equal "Week day" thu (week-day (date year: 2022 month: 06 day: 23)))

(test-group "week-1-start"
  (test-equal
      (date year: 2019 month: 12 day: 30)
    (week-1-start (date year: 2020)
                  mon))
  (test-equal
      (date year: 2018 month: 1 day: 1)
    (week-1-start (date year: 2018)
                  mon))

  (test-equal
      (date year: 2017 month: 1 day: 2)
    (week-1-start (date year: 2017 month: 1 day: 1)
                  mon)))

(test-group "week-number"
  (test-equal "Week number at end of year"   53 (week-number (date year: 2008 month: 12 day: 31) sun))
  (test-equal "Week number at start of year" 53 (week-number (date year: 2009 month: 01 day: 01) sun))
  (test-equal "Week using next years weeks"   1 (week-number (date year: 2018 month: 12 day: 31) mon)))

(test-equal (date year: 2008 month: 12 day: 28) (date-starting-week 53 (date year: 2008) sun))
(test-equal (date year: 2007 month: 12 day: 30) (date-starting-week 1  (date year: 2008) sun))


;; TODO timespans can be both date, times, and datetimes
;; Check those cases?
(test-group "Overlapping timespans"
  ;;    A          B          C          D          E         ¬F
  ;; |s1|     :     |s2| : |s1|     :     |s2| :          : |s1|
  ;; |  |     :     |  | : |  ||s2| : |s1||  | : |s1||s2| : |  |
  ;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | : |  ||  | :
  ;;     |  | : |  |     : |  ||  | : |  ||  | : |  ||  | :     |s2|
  ;;     |  | : |  |     : |  |     :     |  | :          :     |  |
  (test-assert "[A] End of S1 overlaps start of S2"
    (timespan-overlaps? (datetime hour: 10) (datetime hour: 12)
                        (datetime hour: 11) (datetime hour: 13)))
  (test-assert "[B] Start of S1 overlaps end of S2"
    (timespan-overlaps? (datetime hour: 11) (datetime hour: 13)
                        (datetime hour: 10) (datetime hour: 12)))
  (test-assert "[C] S1 complete encompasses S2"
    (timespan-overlaps? (datetime hour: 10) (datetime hour: 13)
                        (datetime hour: 11) (datetime hour: 12)))
  (test-assert "[D] S2 complete encompasses S1"
    (timespan-overlaps? (datetime hour: 11) (datetime hour: 12)
                        (datetime hour: 10) (datetime hour: 13)))
  (test-assert "[E] S1 is equal to S2"
    (timespan-overlaps? (datetime hour: 11) (datetime hour: 12)
                        (datetime hour: 11) (datetime hour: 12)))
  (test-assert "[F] S1 dosesn't overlap S2"
    (not
     (timespan-overlaps? (datetime hour: 10) (datetime hour: 11)
                         (datetime hour: 12) (datetime hour: 13))))
  (test-assert "If the events only share an instant they don't overlap"
    (not
     (timespan-overlaps? (datetime hour: 10) (datetime hour: 12)
                         (datetime hour: 12) (datetime hour: 14)))))

(test-equal "weekday-list" (list wed thu fri sat sun mon tue) (weekday-list wed))
(test-equal "start of week" (date year: 2022 month: 06 day: 20) (start-of-week (date year: 2022 month: 06 day: 23) mon))
(test-equal "end of week"   (date year: 2022 month: 06 day: 26) (end-of-week (date year: 2022 month: 06 day: 23) mon))


(test-group "month-days"
  (call-with-values (lambda () (month-days (date year: 2022 month: 06 day: 01) mon))
    (lambda (before actual after)
      (test-equal "before" (list (date year: 2022 month: 05 day: 30) (date year: 2022 month: 05 day: 31))             before)
      (test-equal "actual" (stream->list 30 (day-stream (date year: 2022 month: 06 day: 01))) actual)
      (test-equal "after"  (list (date year: 2022 month: 07 day: 01) (date year: 2022 month: 07 day: 02) (date year: 2022 month: 07 day: 03)) after))))

(test-group "Days in interval"
  (test-equal "Steps from start to end of month" 31 (days-in-interval (date year: 2022 month: 01 day: 01) (date year: 2022 month: 01 day: 31)))
  (test-error "Negative intervals should fail" 'misc-error (days-in-interval (date year: 2022 month: 01 day: 01) (date year: 2020 month: 01 day: 31))))

(test-equal "Year day" 191 (year-day (date year: 2020 month: 07 day: 09)))

(test-group "Convertions to decimal time"
  (test-group "Time->decimal-hour"
    (test-equal "Exact number of hours is whole number" 5.0 (time->decimal-hour (time hour: 5)))
    (test-equal "Minutes are \"base\" 60"               5.5 (time->decimal-hour (time hour: 5 minute: 30)))
    (test-equal "60 Minutes gives a whole hour"         6.0 (time->decimal-hour (time hour: 5 minute: 60)))
    (test-equal "A second is the right length" (/ 1.0 3600) (time->decimal-hour (time second: 1))))

  (test-group "Datetime->decimal-hour"
    (test-equal "Datetimes without dates work as times"
      5.5 (datetime->decimal-hour (datetime hour: 5 minute: 30)))
    (test-equal "Full day" 24.0 (datetime->decimal-hour (datetime day: 1)))
    (test-error "Can't get length of month without information about which month"
      'misc-error (datetime->decimal-hour (datetime month: 1)))
    (test-equal "Can get length of month if we have a month"
      (* 31 24.0) (datetime->decimal-hour (datetime month: 1) (date year: 2020 month: 01 day: 01)))))

(test-equal "date-range"
  (list (date year: 2020 month: 01 day: 01)
        (date year: 2020 month: 01 day: 03)
        (date year: 2020 month: 01 day: 05)
        (date year: 2020 month: 01 day: 07)
        (date year: 2020 month: 01 day: 09)
        (date year: 2020 month: 01 day: 11)
        (date year: 2020 month: 01 day: 13)
        (date year: 2020 month: 01 day: 15)
        (date year: 2020 month: 01 day: 17)
        (date year: 2020 month: 01 day: 19)
        (date year: 2020 month: 01 day: 21)
        (date year: 2020 month: 01 day: 23)
        (date year: 2020 month: 01 day: 25)
        (date year: 2020 month: 01 day: 27)
        (date year: 2020 month: 01 day: 29)
        (date year: 2020 month: 01 day: 31)
        (date year: 2020 month: 02 day: 2))
  (date-range (date year: 2020 month: 1 day: 1)
              (date year: 2020 month: 2 day: 2)
              2))




(test-group "Equals"
  ;; date=?, time=?, and datetime=? are alias to their non-question-mark
  ;; alternatives. Using them interchangably below.
  (test-group "date"
    (test-assert "Zero dates are all equal"
      (date=))
    (test-assert "A single date is equal to itself"
      (date=? (date year: 2020 month: 10 day: 20)))
    (test-assert "Two dates are equal to each other"
      (date= (date year: 2020 month: 10 day: 20) (date year: 2020 month: 10 day: 20)))
    (test-assert "Two dates which are NOT equal to each other"
      (not (date= (date year: 2020 month: 10 day: 20) (date year: 2020 month: 10 day: 21))))
    (test-assert "More than two dates which are all equal"
      (date=? (date year: 2020 month: 10 day: 20) (date year: 2020 month: 10 day: 20) (date year: 2020 month: 10 day: 20))))

  (test-group "time"
    (test-assert "Zero times are all equal"
      (time=))
    (test-assert "A single time is equal to itself"
      (time=? (time hour: 20 minute: 30 second: 40)))
    (test-assert "Two times are equal to each other"
      (time= (time hour: 20 minute: 30 second: 40) (time hour: 20 minute: 30 second: 40)))
    (test-assert "Two times which are NOT equal to each other"
      (not (time= (time hour: 20 minute: 30 second: 40) (time hour: 10 minute: 30 second: 40))))
    (test-assert "More than two times which are all equal"
      (time=? (time hour: 20 minute: 30 second: 40) (time hour: 20 minute: 30 second: 40) (time hour: 20 minute: 30 second: 40))))

  (test-group "Datetime"
    (test-assert "Zero datetimes \"all\" are equal"
      (datetime=))
    (test-assert "A single datetime is equal to itself"
      (datetime= (datetime)))
    (test-assert "Two equal datetimes are equal"
      (datetime= (datetime hour: 1) (datetime hour: 1)))
    (test-assert "Two dissimmalar datetimes aren't equal"
      (not (datetime= (datetime hour: 1) (datetime hour: 2))))

    (test-error "Can't compare datetimes of differing timezones"
      'wrong-type-arg
      (datetime= (datetime) (datetime tz: "Something Else")))

    (test-assert "Three equal datetimes are equal"
      (datetime= (datetime hour: 1) (datetime hour: 1) (datetime hour: 1)))))

(test-group "Comparisons"
  (test-group "Zero arguments"
    (test-group "Dates"
      (test-assert "zero dates are greater" (date<))
      (test-assert "zero dates are less"    (date>)))
    (test-group "Times"
      (test-assert "zero times are greater" (time<))
      (test-assert "zero times are less"    (time>)))
    (test-group "Datetimes"
      (test-assert "zero datetimes are greater" (datetime<))
      (test-assert "zero datetimes are less"    (datetime>))))

  (test-group "Single argument"
    (test-group "Dates"
      (test-assert "one date are greater" (date< (date)))
      (test-assert "one date are less"    (date> (date))))
    (test-group "Times"
      (test-assert "one time are greater" (time< (time)))
      (test-assert "one time are less"    (time> (time))))
    (test-group "Datetimes"
      (test-assert "one datetime are greater" (datetime< (datetime)))
      (test-assert "one datetime are less"    (datetime> (datetime)))))


  (test-group "Two arguments"
    (test-group "Dates"
      (test-assert "positive comparison"      (date< (date day: 1) (date day: 2)))
      (test-assert "negative comparison" (not (date> (date day: 1) (date day: 2)))))
    (test-group "Times"
      (test-assert "positive comparison"      (time< (time hour: 1) (time hour: 2)))
      (test-assert "negative comparison" (not (time> (time hour: 1) (time hour: 2)))))
    (test-group "Datetimes"
      (test-assert "positive comparison"      (datetime< (datetime day: 1) (datetime day: 2)))
      (test-assert "negative comparison" (not (datetime> (datetime day: 1) (datetime day: 2))))))

  (test-group "Two arguments"
    (test-group "Dates"
      (test-assert "positive comparison"
        (date< (date day: 1) (date day: 2) (date day: 3)))
      (test-assert "negative comparison"
        (not (date< (date day: 1) (date day: 2) (date day: 1)))))
    (test-group "Times"
      (test-assert "positive comparison"
        (time< (time hour: 1) (time hour: 2) (time hour: 3)))
      (test-assert "negative comparison"
        (not (date< (date day: 1) (date day: 2) (date day: 1)))))
    (test-group "Datetimes"
      (test-assert "positive comparison"
        (datetime< (datetime day: 1) (datetime day: 2) (datetime day: 3)))
      (test-assert "negative comparison"
        (not (datetime< (datetime day: 1) (datetime day: 2) (datetime day: 1)))))))

;; TODO
date<=
time<=
datetime<=

(test-group "Arithmetic"
  (test-group "Date"
    (test-group "Unary application"
      (test-equal "Date+ single argument returns itself" (date) (date+ (date)))
      (test-equal "Date- single argument returns itself" (date) (date- (date))))

    (test-group "Simple cases"
      (test-group "Days"
        (test-equal "Add"     (date year: 2020 month: 01 day: 06) (date+ (date year: 2020 month: 01 day: 01) (date day: 5)))
        (test-equal "Remove"  (date year: 2020 month: 01 day: 01) (date- (date year: 2020 month: 01 day: 06) (date day: 5))))
      (test-group "Months"
        (test-equal "Add"     (date year: 2020 month: 06 day: 01) (date+ (date year: 2020 month: 01 day: 01) (date month: 5)))
        (test-equal "Remove"  (date year: 2020 month: 01 day: 01) (date- (date year: 2020 month: 06 day: 01) (date month: 5))))
      (test-group "Years"
        (test-equal "Add"     (date year: 2022 month: 01 day: 01) (date+ (date year: 2020 month: 01 day: 01) (date year: 2)))
        (test-equal "Remove"  (date year: 2020 month: 01 day: 01) (date- (date year: 2022 month: 01 day: 01) (date year: 2)))))

    (test-group "Many operands"
      (test-equal (date year: 2021 month: 02 day: 02)
          (date+ (date year: 2020 month: 01 day: 01)
                 (date day: 1)
                 (date month: 1)
                 (date year: 1))))

    (test-group "Overflow"
      ;; Years don't overflow, so no need to test
      (test-equal "Day overflow"        (date year: 2022 month: 02 day: 01) (date+ (date year: 2022 month: 01 day: 31) (date day: 1)))
      (test-equal "Month overflow"      (date year: 2023 month: 01 day: 01) (date+ (date year: 2022 month: 12 day: 01) (date month: 1)))
      (test-equal "Date+Month overflow" (date year: 2023 month: 01 day: 01) (date+ (date year: 2022 month: 12 day: 31) (date day: 1))))

    ;; NOTE
    (test-equal "Undefined overflow"
      (date year: 2020 month: 02 day: 31)
      (date+ (date year: 2020 month: 01 day: 31) (date month: 1)))
    )

  (test-group "Time"
    (test-group "Unary application"
      (test-equal "Time+ single argument returns itself" (time) (time+ (time)))
      (test-equal "Time- single argument returns itself" (time) (time- (time))))

    (test-group "Simple cases"
      (test-group "Seconds"
        (test-equal "Add"     (time hour: 20 minute: 00 second: 40) (time+ (time hour: 20 minute: 00 second: 00) (time second: 40)))
        (test-equal "Remove"  (time hour: 20 minute: 00 second: 00) (time- (time hour: 20 minute: 00 second: 40) (time second: 40))))
      (test-group "Minutes"
        (test-equal "Add"     (time hour: 20 minute: 10 second: 00) (time+ (time hour: 20 minute: 00 second: 00) (time minute: 10)))
        (test-equal "Remove"  (time hour: 20 minute: 00 second: 00) (time- (time hour: 20 minute: 10 second: 00) (time minute: 10))))
      (test-group "Hours"
        (test-equal "Add"     (time hour: 22 minute: 00 second: 00) (time+ (time hour: 20 minute: 00 second: 00) (time hour: 2)))
        (test-equal "Remove"  (time hour: 20 minute: 00 second: 00) (time- (time hour: 22 minute: 00 second: 00) (time hour: 2)))))

    (test-group "Overflowing cases"
      (test-group "Addition"
        (test-group "Single overflow"
          (call-with-values (lambda () (time+ (time hour: 20 minute: 00 second: 00) (time hour: 5)))
            (lambda (result overflow)
              (test-equal "Time" (time hour: 1) result)
              (test-equal "Overflow" 1 overflow))))
        (test-group "Mulitple overflows"
          (call-with-values (lambda () (time+ (time hour: 20 minute: 00 second: 00) (time hour: 5) (time hour: 24)))
            (lambda (result overflow)
              (test-equal "Time" (time hour: 1) result)
              (test-equal "Overflow" 2 overflow)))))

      (test-group "Subtraction"
        (test-group "Single overflow"
          (call-with-values (lambda () (time- (time hour: 20 minute: 00 second: 00) (time hour: 25)))
            (lambda (result overflow)
              (test-equal "Time" (time hour: 19) result)
              (test-equal "Overflow" 1 overflow))))
        (test-group "Mulitple overflows"
          (call-with-values (lambda () (time- (time hour: 4) (time hour: 10) (time hour: 24)))
            (lambda (result overflow)
              (test-equal "Time" (time hour: 18) result)
              (test-equal "Overflow" 2 overflow))))))))

;; TODO
datetime+ datetime-

;;; TODO document this behaviour
(test-equal "(datetime+ x 0) causes overflow handling"
  (datetime year: 2020 month: 3 day: 1)
  (datetime+ (datetime year: 2020 month: 2 day: 30) (datetime)))

(test-group "Date difference"
  (test-assert "The differente between a date and itself is zero"
    (date-zero? (date-difference (date year: 2022 month: 02 day: 02) (date year: 2022 month: 02 day: 02))))

  (test-error "Later date must be first" 'misc-error
              (date-difference (date year: 2020 month: 01 day: 01) (date year: 2021 month: 01 day: 01)))

  (test-error "Negative months are invalid" 'misc-error
              (date-difference (date) (date month: -1)))
  (test-error "Negative days are invalid" 'misc-error
              (date-difference (date) (date day: -1)))
  (test-equal "Negative years ARE valid"
    (date year: 1) (date-difference (date) (date year: -1)))

  ;; TODO, the following returns (date month: 2 day: 3), which is
  ;; clearly not right
  (date-difference #2026-05-01 #2026-02-28)
  )

;; TODO
datetime-difference


'((datetime core))

(define-module (test datetime)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((srfi srfi-41)
               :select (stream->list stream-take))
  :use-module (datetime)
  :use-module ((ice-9 format) :select (format))
  :use-module ((ice-9 i18n) :select (make-locale))
  :use-module ((guile) :select (LC_CTYPE LC_TIME)))

;;; Skipped since the code generating the (expected) error is disabled, due to
;;; optional fields at the end of string. See the (null? str) case is
;;; datetime->string
(test-expect-fail "Premature end of string to parse")

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
    (let ((get-time% (@@ (datetime) get-time%)))
      (test-group "Empty datetime"
        (let ((dt (datetime)))
          ;; TODO figure out propper export of get-time%
          (test-assert "Datetime date is date" (date? (get-date dt)))
          (test-assert "Datetime date is zero" (date-zero? (get-date dt)))
          (test-assert "Datetime time is time" (time? (get-time% dt)))
          (test-assert "Datetime time is zero" (time-zero? (get-time% dt)))
          (test-eqv "Defalut timezone is #f" #f (get-timezone dt))))

      (test-group "Datetime with keys"
        (let ((dt (datetime date: (date day: 10)
                            time: (time minute: 20))))
          (test-equal "Given date is stored"
            10 (day (get-date dt)))
          (test-equal "Given time is stored"
            20 (minute (get-time% dt))))
        (test-error "Date must be a date" 'wrong-type-arg (datetime date: 1))
        (test-error "Date must be a date" 'wrong-type-arg (datetime date: (time)))
        (test-assert "Date: #f gives still constructs a date" (date? (get-date (datetime date: #f))))
        (test-error "Time must be a time" 'wrong-type-arg (datetime time: 1))
        (test-error "Time must be a time" 'wrong-type-arg (datetime time: (date)))
        (test-assert "Time: #f gives still constructs a time" (time? (get-time% (datetime time: #f))))

        (let ((dt (datetime hour: 20 day: 30)))
          (test-equal "Time objects can be implicitly created" 20 (hour (get-time% dt)))
          (test-equal "Date objects can be implicitly created" 30 (day (get-date dt))))
        (let ((dt (datetime day: 30 time: (time hour: 20))))
          (test-equal "\"Upper\" and \"lower\" keys can be mixed"
            20 (hour (get-time% dt)))
          (test-equal "\"Upper\" and \"lower\" keys can be mixed"
            30 (day (get-date dt))))

        (let ((dt (datetime hour: 30 time: (time hour: 20))))
          (test-equal "time: has priority over hour: (and the like)"
            20 (hour (get-time% dt)))))
      (let ((dt (datetime day: 30 date: (date day: 20))))
        (test-equal "date: has priority over day: (and the like)"
          20 (day (get-date dt)))))))

;; Before the general parser, since it's a dependency string->datetime.
(test-group "Parse Month"

  (test-equal "Parse full month name" jan (parse-month "January" en_US))
  (test-equal "Parse full weird case" jan (parse-month "jaNuaRy" en_US))
  (test-equal "Parse partial month name" jan (parse-month "Jan" en_US))
  (test-equal "Failing parse of month name" -1 (parse-month "Unknown" en_US))
  (test-equal "Overlap gives earliest month" mar (parse-month "m" en_US))

  (test-equal "Parse month with different locale" may (parse-month "maj" sv_SE)))


(test-group "Parser"
  (test-group "Simple individual rules"
    (test-group "Year"
      (test-equal "~Y year"  (datetime year: 2020)  (string->datetime "2020" "~Y"))
      (test-equal "~Y year single digit"  (datetime year: 2)  (string->datetime "2" "~Y"))
      (test-equal "~Y year leading zero"  (datetime year: 2)  (string->datetime "02" "~Y"))
      (test-error "~Y parses at max four digits" 'misc-error (string->datetime "14411" "~Y")))

    (test-group "Month"
      (test-equal "~m month"  (datetime month: 10)  (string->datetime "10" "~m"))
      (test-equal "~m month single digit"  (datetime month: 1)  (string->datetime "1" "~m"))
      (test-equal "~m month leading zero"  (datetime month: 1)  (string->datetime "01" "~m"))
      (test-error "~m parses at max two digits" 'misc-error (string->datetime "111" "~m")))

    ;; Extra tests are skipped for these, since they are shared with Month
    (test-equal "~d day"    (datetime day: 20)    (string->datetime "20" "~d"))
    (test-equal "~H hour"   (datetime hour: 15)   (string->datetime "15" "~H"))
    (test-equal "~M minute" (datetime minute: 30) (string->datetime "30" "~M"))
    (test-equal "~S second" (datetime second: 59) (string->datetime "59" "~S")))


  (test-equal "Literal character" (datetime) (string->datetime "T" "T"))
  (test-equal "~~ '~'" (datetime) (string->datetime "~" "~~"))
  (test-error "Mismatched literal ~" 'misc-error (string->datetime "A" "~~"))

  (test-error "Stray ~ at end of fmt" 'misc-error (string->datetime "~" "~"))
  (test-error "Stray ~ in middle of fmt" 'misc-error (string->datetime "~ 1" "~ ~d"))
  (test-error "Unknown escape" 'misc-error (string->datetime "10" "~x"))
  (test-error "Premature end of string to parse" 'misc-error (string->datetime "" "~Y"))
  (test-error "Wrong Literal character" 'misc-error (string->datetime "T" "Z"))


  ;; Does the parser continue correctly
  (test-group "Tokens following each other"
    (test-equal "Year indirectly followed by month"
      (datetime year: 2020 month: 1)
      (string->datetime "2020-01" "~Y-~m"))
    ;; Does the parser handle tokens without delimiters, instead going by their max size
    (test-equal "Year directly follewed by month"
      (datetime year: 2020 month: 1)
      (string->datetime "202001" "~Y~m")))


  (test-group "Timezone"
    (test-equal "~Z 'Z'"
      (datetime tz: "UTC") (string->datetime "Z" "~Z"))
    (test-equal "~Z Is optional"
      (datetime) (string->datetime "" "~Z"))
    (test-equal "~Z Is optional with stuff after"
      (datetime hour: 20) (string->datetime "20" "~Z~H"))
    ;; This was earlier a bug
    (test-equal "Zoneinfo is kept while not at end"
      (datetime year: 2020 tz: "UTC")
      (string->datetime "Z2020" "~Z~Y")))


  (test-group "Month by name"
    ;; ~b, ~B, and ~h all does the same thing, and exists for symmetry with
    ;; datetime->string (where they don't do the exact same thing). Each is used
    ;; at least once below to ensure that they all work.
    (test-equal "Standalone month, and at end"
      (datetime month: 1)
      (string->datetime "Jan" "~b" en_US))

    ;; Separate test from above, since month does the check itself
    (test-error "Stray ~ after month"
      'misc-error (string->datetime "Jan" "~b~" en_US))

    (test-equal "Month with explicit ~ after"
      (datetime month: mar)
      (string->datetime "M~" "~B~~" en_US))

    (test-error "Month with other specifier directly after"
      'misc-error (string->datetime "January" "~b~b"))

    (test-equal "Month with other explict char after"
      (datetime month: mar)
      (string->datetime "Mar|" "~h|" en_US))

    (test-equal "Locale information is used"
      (datetime month: may)
      (string->datetime "Maj" "~h" sv_SE)))

  ;; TODO AM/PM string ~p

  (test-group "Complete parses"
    (test-equal "Parse complete ISO date"
      (datetime year: 2020 month: 3 day: 10)
      (string->datetime "2020-03-10" "~Y-~m-~d"))

    (test-equal "Parse complete ISO time"
      (datetime hour: 10 minute: 20 second: 30)
      (string->datetime "10:20:30" "~H:~M:~S"))

    (test-equal "Parse complete ISO date-time"
      (datetime year: 2020 month: 3 day: 10
                hour: 10 minute: 20 second: 30)
      (string->datetime "2020-03-10T10:20:30"
                        "~Y-~m-~dT~H:~M:~S")))

  (test-group "string->datetime default format-specifier"
    (test-equal "Default date-time format-specifier takes ISO date-times"
      (datetime year: 2020 month: 3 day: 10
                hour: 10 minute: 20 second: 30)
      (string->datetime "2020-03-10T10:20:30"))

    (test-equal "Default date-time format-specifier takes ISO date-times (with zone)"
      (datetime year: 2020 month: 3 day: 10
                hour: 10 minute: 20 second: 30
                tz: "UTC")
      (string->datetime "2020-03-10T10:20:30Z")))


  (test-group "string->time"
    (test-assert "String->time returns time objects"
      (time? (string->time "10" "~H")))

    (test-equal "String->time complete parse"
      (time hour: 10 minute: 20 second: 30)
      (string->time "10:20:30" "~H:~M:~S"))

    (test-equal "String->time complete parse, default format-specifier"
      (time hour: 10 minute: 20 second: 30)
      (string->time "10:20:30")))

  (test-group "string->date"
    (test-assert "String->date returns time objects"
      (date? (string->date "10" "~Y")))

    (test-equal "String->date complete parse"
      (date year: 2020 month: 3 day: 10)
      (string->date "2020-03-10" "~Y-~m-~d"))

    (test-equal "String->date complete parse, default format-specifier"
      (date year: 2020 month: 3 day: 10)
      (string->date "2020-03-10")))

  (test-group "Pre-specified parsers"
    (test-group "ICS (RFC 5545)"
      (test-equal "date"
        (date year: 2020 month: 10 day: 20)
        (parse-ics-date "20201020"))
      (test-equal "time"
        (time hour: 10 minute: 20 second: 30)
        (parse-ics-time "102030"))
      (test-equal "datetime"
        (datetime year: 2020 month: 10 day: 20
                  hour: 10 minute: 20 second: 30)
        (parse-ics-datetime "20201020T102030"))
      (test-equal "datetime (with zone)"
        (datetime year: 2020 month: 10 day: 20
                  hour: 10 minute: 20 second: 30
                  tz: "UTC")
        (parse-ics-datetime "20201020T102030Z")))

    (test-group "ISO"
      (test-equal "date"
        (date year: 2020 month: 10 day: 20)
        (parse-iso-date "2020-10-20"))
      (test-equal "time"
        (time hour: 10 minute: 20 second: 30)
        (parse-iso-time "10:20:30"))
      (test-equal "datetime"
        (datetime year: 2020 month: 10 day: 20
                  hour: 10 minute: 20 second: 30)
        (parse-iso-datetime "2020-10-20T10:20:30")))

    ;; Parse freeform date
    )

  (test-group "string->date/-time"
    (test-equal "Date like gives date"
      (date year: 2020 month: 10 day: 20)
      (string->date/-time "2020-10-20"))
    (test-equal "Time like gives time"
      (time hour: 10 minute: 20 second: 30)
      (string->date/-time "10:20:30"))
    (test-equal "Datetime like gives datetime"
      (datetime year: 2020 month: 10 day: 20
                hour: 10 minute: 20 second: 30)
      (string->date/-time "2020-10-20T10:20:30"))

    ;; These are disabled since trailing fmt is allowed
    ;; (test-error "Bad date-like crashes"
    ;;   'misc-error (string->date/-time "2020-10"))
    ;; (test-error "Bad time-like crashes"
    ;;   'misc-error (string->date/-time "20:10"))
    (test-error "Really bad crashes"
      'misc-error (string->date/-time "Hello"))
    ))


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
  1656005146 (datetime->unix-time #2022-06-23T17:25:46Z))

(test-equal "Datetime->unix-time before epoch"
  -62167219200
  (datetime->unix-time #0000-01-01T00:00:00Z))

(test-equal "unix-time->datetime" #2020-09-13T12:26:40Z
            (unix-time->datetime 1600000000))
(test-equal "unix-time->datetime on epoch" #1970-01-01T00:00:00Z
            (unix-time->datetime 0))
(test-equal "unix-time->datetime before epoch" #1919-04-20T11:33:20Z
            (unix-time->datetime -1600000000))

;; (unix-time->datetime (expt 2 31)) ; => #2038-01-19T03:14:08Z
;; (unix-time->datetime (1+ (expt 2 31))) ; => #2038-01-19T03:14:09Z
;; (unix-time->datetime (- (expt 2 31))) ; => #1901-12-13T20:45:52Z


(test-assert "Current datetime returns a datetime"
  (datetime? (current-datetime)))
(test-equal "Current datetime returns with tz: UTC"
  "UTC" (get-timezone (current-datetime)))
(test-assert "Current-date returns a date"
  (date? (current-date)))


;; TODO write these, also, check connection to get-time%
get-datetime
as-date
as-time
as-datetime

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

(test-equal "Start of month" #2020-01-01 (start-of-month #2020-01-15))
(test-equal "End of month" #2000-02-29 (end-of-month #2000-02-01))

(test-equal "Start of year" #2020-01-01 (start-of-year #2020-12-31))
;; Note that end-of-year (apparently) doesn't exist

(test-group "Date streams"
  (test-equal "Day stream"
    (list #2020-01-01 #2020-01-02 #2020-01-03 #2020-01-04 #2020-01-05)
    (stream->list 5 (day-stream #2020-01-01)))
  (test-equal "Week stream"
    (list #2020-01-01 #2020-02-01 #2020-03-01 #2020-04-01 #2020-05-01)
    (stream->list 5 (month-stream #2020-01-01)))
  (test-equal "Month stream"
    (list #2020-01-01 #2020-01-08 #2020-01-15 #2020-01-22 #2020-01-29)
    (stream->list 5 (week-stream #2020-01-01))))

;; See time< tests for more context
(test-group "Min/max"
  (test-equal "Time min"
    #07:40:50 (time-min #10:20:30 #07:40:50))
  (test-equal "Time max"
    #10:20:30 (time-max #10:20:30 #07:40:50))

  (test-equal "Date min"
    #2020-02-02 (date-min #2020-02-02 #2020-03-01))
  (test-equal "Date max"
    #2020-03-01 (date-max #2020-02-02 #2020-03-01))

  (test-equal "Datetime min"
    #2020-02-02T10:20:30 (datetime-min #2020-02-02T10:20:30 #2020-03-01T07:40:50))
  (test-equal "Datetime max"
    #2020-03-01T07:40:50 (datetime-max #2020-02-02T10:20:30 #2020-03-01T07:40:50)))

(test-equal "Week day" thu (week-day #2022-06-23))

(test-equal "week-1-start" #2019-12-30 (week-1-start #2020-01-01 mon))

;; Possibly add case where the end of the year uses next years week numbers
(test-equal "Week number at end of year"   53 (week-number #2008-12-31 sun))
(test-equal "Week number at start of year" 53 (week-number #2009-01-01 sun))

(test-equal #2008-12-28 (date-starting-week 53 (date year: 2008) sun))
(test-equal #2007-12-30 (date-starting-week 1  (date year: 2008) sun))

(test-group "Week day name"
  (test-equal "Simple" "Saturday" (week-day-name sat locale: en_US))
  (test-equal "Truncated" "Sa" (week-day-name sat 2 locale: en_US))
  (test-equal "Other locale" "lördag" (week-day-name sat locale: sv_SE))
  (test-equal "Other locale, truncated" "lö" (week-day-name sat 2 locale: sv_SE)))

;; TODO timespans can be both date, times, and datetimes
;; Check those cases?
(test-group "Overlapping timespans"
  ;;    A          B          C          D          E         ¬F
  ;; |s1|     :     |s2| : |s1|     :     |s2| :          : |s1|
  ;; |  |     :     |  | : |  ||s2| : |s1||  | : |s1||s2| : |  |
  ;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | : |  ||  | :
  ;;     |  | : |  |     : |  ||  | : |  ||  | : |  ||  | :     |s2|
  ;;     |  | : |  |     : |  |     :     |  | :          :     |  |
  (test-assert "End of S1 overlaps start of S2"
    (timespan-overlaps? #10:00:00 #12:00:00
                        #11:00:00 #13:00:00))
  (test-assert "Start of S1 overlaps end of S2"
    (timespan-overlaps? #11:00:00 #13:00:00
                        #10:00:00 #12:00:00))
  (test-assert "S1 complete encompasses S2"
    (timespan-overlaps? #10:00:00 #13:00:00
                        #11:00:00 #12:00:00))
  (test-assert "S2 complete encompasses S1"
    (timespan-overlaps? #11:00:00 #12:00:00
                        #10:00:00 #13:00:00))
  (test-assert "S1 is equal to S2"
    (timespan-overlaps? #11:00:00 #12:00:00
                        #11:00:00 #12:00:00))
  (test-assert "S1 dosesn't overlap S2"
    (not
     (timespan-overlaps? #10:00:00 #11:00:00
                         #12:00:00 #13:00:00)))
  (test-assert "If the events only share an instant they don't overlap"
    (not
     (timespan-overlaps? #10:00:00 #12:00:00
                         #12:00:00 #14:00:00))))

(test-equal #2022-06-25 (find-first-week-day sat #2022-06-23))

(test-group "All weekdays in <>"
  (test-equal "month, if starting from beginning of month"
    (list #2022-06-03 #2022-06-10 #2022-06-17 #2022-06-24)
    (all-wday-in-month fri #2022-06-01))

  (test-equal "month, if starting from the middle"
    (list #2022-06-24)
    (all-wday-in-month fri #2022-06-23))

  (test-equal "year, if starting from the beggining"
    (list #2022-01-07 #2022-01-14 #2022-01-21 #2022-01-28 #2022-02-04 #2022-02-11 #2022-02-18 #2022-02-25 #2022-03-04 #2022-03-11 #2022-03-18 #2022-03-25 #2022-04-01 #2022-04-08 #2022-04-15 #2022-04-22 #2022-04-29 #2022-05-06 #2022-05-13 #2022-05-20 #2022-05-27 #2022-06-03 #2022-06-10 #2022-06-17 #2022-06-24 #2022-07-01 #2022-07-08 #2022-07-15 #2022-07-22 #2022-07-29 #2022-08-05 #2022-08-12 #2022-08-19 #2022-08-26 #2022-09-02 #2022-09-09 #2022-09-16 #2022-09-23 #2022-09-30 #2022-10-07 #2022-10-14 #2022-10-21 #2022-10-28 #2022-11-04 #2022-11-11 #2022-11-18 #2022-11-25 #2022-12-02 #2022-12-09 #2022-12-16 #2022-12-23 #2022-12-30)
    (all-wday-in-year fri #2022-01-01))

  (test-equal "year, if starting from the middle"
    (list #2022-06-03 #2022-06-10 #2022-06-17 #2022-06-24 #2022-07-01 #2022-07-08 #2022-07-15 #2022-07-22 #2022-07-29 #2022-08-05 #2022-08-12 #2022-08-19 #2022-08-26 #2022-09-02 #2022-09-09 #2022-09-16 #2022-09-23 #2022-09-30 #2022-10-07 #2022-10-14 #2022-10-21 #2022-10-28 #2022-11-04 #2022-11-11 #2022-11-18 #2022-11-25 #2022-12-02 #2022-12-09 #2022-12-16 #2022-12-23 #2022-12-30)
    (all-wday-in-year fri #2022-06-01)))

;; TODO
in-date-range?

(test-equal "weekday-list" (list wed thu fri sat sun mon tue) (weekday-list wed))
(test-equal "start of week" #2022-06-20 (start-of-week #2022-06-23 mon))
(test-equal "end of week"   #2022-06-26 (end-of-week #2022-06-23 mon))


(test-group "month-days"
  (call-with-values (lambda () (month-days #2022-06-01 mon))
    (lambda (before actual after)
      (test-equal "before" (list #2022-05-30 #2022-05-31)             before)
      (test-equal "actual" (stream->list 30 (day-stream #2022-06-01)) actual)
      (test-equal "after"  (list #2022-07-01 #2022-07-02 #2022-07-03) after))))

(test-group "Days in interval"
  (test-equal "Steps from start to end of month" 31 (days-in-interval #2022-01-01 #2022-01-31))
  (test-error "Negative intervals should fail" 'misc-error (days-in-interval #2022-01-01 #2020-01-31)))

(test-equal "Year day" 191 (year-day #2020-07-09))

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
      (* 31 24.0) (datetime->decimal-hour (datetime month: 1) #2020-01-01))))

;; TODO
date-range

(test-group "To string"
  (test-group "Datetime->string"
    (test-equal "A letter becomes itself"
      "H" (datetime->string (datetime) "H"))
    (test-group "Single rules"
      (test-equal "~" (datetime->string (datetime) "~~"))
      (test-equal "01" (datetime->string (datetime hour: 1)   "~H"))
      (test-equal " 1" (datetime->string (datetime hour: 1)   "~k"))
      (test-equal "02" (datetime->string (datetime minute: 2) "~M"))
      (test-equal "03" (datetime->string (datetime second: 3) "~S"))
      (test-equal "0002" (datetime->string (datetime year: 2) "~Y"))
      (test-equal "02" (datetime->string (datetime month: 2)  "~m"))
      (test-equal "04" (datetime->string (datetime day: 4)    "~d"))
      (test-equal " 4" (datetime->string (datetime day: 4)    "~e"))
      (test-equal "1600000000" (datetime->string #2020-09-13T12:26:40Z "~s"))

      (test-equal "2022-10-20" (datetime->string (datetime date: #2022-10-20) "~1"))
      (test-equal "10:20:30"   (datetime->string (datetime time: #10:20:30)   "~3"))

      (test-group "Locale dependant (en_US)"
        (test-equal "Saturday" (datetime->string (datetime date: (find-first-week-day sat #2020-01-01)) "~A" en_US))
        (test-equal "Sat"      (datetime->string (datetime date: (find-first-week-day sat #2020-01-01)) "~a" en_US))
        (test-equal "January"  (datetime->string (datetime date: (date month: 1)) "~B" en_US))
        (test-equal "Jan"      (datetime->string (datetime date: (date month: 1)) "~b" en_US)))

      (test-group "Locale dependant (sv_SE)"
        (test-equal "lördag"  (datetime->string (datetime date: (find-first-week-day sat #2020-01-01)) "~A" sv_SE))
        (test-equal "lör"     (datetime->string (datetime date: (find-first-week-day sat #2020-01-01)) "~a" sv_SE))
        (test-equal "januari" (datetime->string (datetime date: (date month: 1)) "~B" sv_SE))
        (test-equal "jan"     (datetime->string (datetime date: (date month: 1)) "~b" sv_SE)))

      (test-group "Timezone"
        (test-equal "Z" (datetime->string (datetime tz: "UTC")           "~Z"))
        (test-equal ""  (datetime->string (datetime tz: #f)              "~Z"))
        (test-equal ""  (datetime->string (datetime tz: "Anything else") "~Z"))))


    (test-equal "Default fomat specifier gives ISO-formatted date"
      "2006-01-02T15:04:05" (datetime->string #2006-01-02T15:04:05))

    (test-group "Invalid specifiers"
      (test-equal "" (datetime->string (datetime) "~x" allow-unknown?: #t))
      (test-error 'misc-error (datetime->string (datetime) "~x")))

    (test-group "Print syntax for datatypes"
      (test-equal "Date writer" "#2020-01-02" (with-output-to-string (lambda () (write #2020-01-02))))
      (test-equal "Time writer" "#20:30:40"   (with-output-to-string (lambda () (write #20:30:40))))
      (test-equal "Datetime writer"           "#2020-01-02T20:30:40"  (with-output-to-string (lambda () (write #2020-01-02T20:30:40))))
      (test-equal "Datetime writer (with tz)" "#2020-01-02T20:30:40Z" (with-output-to-string (lambda () (write #2020-01-02T20:30:40Z))))))

  ;; Really basic tests, since these are rather thin wrappers
  (test-equal "date->string" "0000-00-00" (date->string (date)))
  (test-equal "time->string" "00:00:00"   (time->string (time))))

(test-group "Equals"
  ;; date=?, time=?, and datetime=? are alias to their non-question-mark
  ;; alternatives. Using them interchangably below.
  (test-group "date"
    (test-assert "Zero dates are all equal"
      (date=))
    (test-assert "A single date is equal to itself"
      (date=? #2020-10-20))
    (test-assert "Two dates are equal to each other"
      (date= #2020-10-20 #2020-10-20))
    (test-assert "Two dates which are NOT equal to each other"
      (not (date= #2020-10-20 #2020-10-21)))
    (test-assert "More than two dates which are all equal"
      (date=? #2020-10-20 #2020-10-20 #2020-10-20)))

  (test-group "time"
    (test-assert "Zero times are all equal"
      (time=))
    (test-assert "A single time is equal to itself"
      (time=? #20:30:40))
    (test-assert "Two times are equal to each other"
      (time= #20:30:40 #20:30:40))
    (test-assert "Two times which are NOT equal to each other"
      (not (time= #20:30:40 #10:30:40)))
    (test-assert "More than two times which are all equal"
      (time=? #20:30:40 #20:30:40 #20:30:40)))

  (test-group "Datetime"
    (test-assert "Zero datetimes \"all\" are equal"
      (datetime=))
    (test-assert "A single datetime is equal to itself"
      (datetime= (datetime)))
    (test-assert "Two equal datetimes are equal"
      (datetime= (datetime hour: 1) (datetime hour: 1)))
    (test-assert "Two dissimmalar datetimes aren't equal"
      (not (datetime= (datetime hour: 1) (datetime hour: 2))))

    ;; NOTE timezone interactions are non-existant
    (test-assert "Two datetimes are equal, regardless of timezone"
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
date/-time< date/-time<? date/-time<= date/-time<=?
date/-time> date/-time>? date/-time>= date/-time>=?

(test-group "Arithmetic"
  (test-group "Date"
    (test-group "Unary application"
      (test-equal "Date+ single argument returns itself" (date) (date+ (date)))
      (test-equal "Date- single argument returns itself" (date) (date- (date))))

    (test-group "Simple cases"
      (test-group "Days"
        (test-equal "Add"     #2020-01-06 (date+ #2020-01-01 (date day: 5)))
        (test-equal "Remove"  #2020-01-01 (date- #2020-01-06 (date day: 5))))
      (test-group "Months"
        (test-equal "Add"     #2020-06-01 (date+ #2020-01-01 (date month: 5)))
        (test-equal "Remove"  #2020-01-01 (date- #2020-06-01 (date month: 5))))
      (test-group "Years"
        (test-equal "Add"     #2022-01-01 (date+ #2020-01-01 (date year: 2)))
        (test-equal "Remove"  #2020-01-01 (date- #2022-01-01 (date year: 2)))))

    (test-group "Many operands"
      (test-equal #2021-02-02
          (date+ #2020-01-01
                 (date day: 1)
                 (date month: 1)
                 (date year: 1))))

    (test-group "Overflow"
      ;; Years don't overflow, so no need to test
      (test-equal "Day overflow"        #2022-02-01 (date+ #2022-01-31 (date day: 1)))
      (test-equal "Month overflow"      #2023-01-01 (date+ #2022-12-01 (date month: 1)))
      (test-equal "Date+Month overflow" #2023-01-01 (date+ #2022-12-31 (date day: 1))))

    ;; NOTE
    (test-equal #2020-02-31 (date+ #2020-01-31 (date month: 1)))
    )

  (test-group "Time"
    (test-group "Unary application"
      (test-equal "Time+ single argument returns itself" (time) (time+ (time)))
      (test-equal "Time- single argument returns itself" (time) (time- (time))))

    (test-group "Simple cases"
      (test-group "Seconds"
        (test-equal "Add"     #20:00:40 (time+ #20:00:00 (time second: 40)))
        (test-equal "Remove"  #20:00:00 (time- #20:00:40 (time second: 40))))
      (test-group "Minutes"
        (test-equal "Add"     #20:10:00 (time+ #20:00:00 (time minute: 10)))
        (test-equal "Remove"  #20:00:00 (time- #20:10:00 (time minute: 10))))
      (test-group "Hours"
        (test-equal "Add"     #22:00:00 (time+ #20:00:00 (time hour: 2)))
        (test-equal "Remove"  #20:00:00 (time- #22:00:00 (time hour: 2)))))

    ))

;; TODO
datetime+ datetime-

(test-group "Date difference"
  (test-assert "The differente between a date and itself is zero"
    (date-zero? (date-difference #2022-02-02 #2022-02-02)))

  (test-error "Later date must be first" 'misc-error
              (date-difference #2020-01-01 #2021-01-01))

  (test-error "Negative months are invalid" 'misc-error
              (date-difference (date) (date month: -1)))
  (test-error "Negative days are invalid" 'misc-error
              (date-difference (date) (date day: -1)))
  (test-equal "Negative years ARE valid"
    (date year: 1) (date-difference (date) (date year: -1))))

;; TODO
datetime-difference

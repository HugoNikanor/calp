;;; Commentary:
;; Tests date, time, and datetime creation,
;; (output) formatting, and arithmetic.
;;; Code:

(define-module (test datetime)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((srfi srfi-41)
               :select (stream->list stream-take))
  :use-module (datetime)
  :use-module ((ice-9 format) :select (format))
  :use-module ((hnh util) :select (let*))
  :use-module ((ice-9 i18n) :select (make-locale))
  :use-module ((guile) :select (LC_CTYPE LC_TIME)))

(test-equal
  "A new time is empty"
  (time)
  #00:00:00)

(test-equal "A new date is empty"
  (date)
  #0000-00-00)

(test-assert
  "Datetime have syntax"
  #2020-01-01)



(test-group "The syntax for <> is equivalent to manually creating them"
  (test-equal
      "dates"
    (date year: 2020 month: 1 day: 1)
    #2020-01-01)

  (test-equal
      "times"
    (time hour: 13 minute: 37 second: 0)
    #13:37:00)

  (test-equal
      "datetimes"
    (datetime year: 2020 month: 1 day: 1
              hour: 13 minute: 37 second: 0)
    #2020-01-01T13:37:00))

(test-equal
    "Date- over leap year month shift"
  #2020-02-28
  (date- #2020-03-05
         (date day: 6)))

(test-equal
    "Date- land on leap day"
  #2020-02-29
  (date- #2020-03-05
         (date day: 5)))

(test-equal
    "Date- within month"
  #2020-03-01
  (date- #2020-03-05
         (date day: 4)))

(test-equal
  "date+ day"
  #2020-10-10
  (date+ #2020-10-01
         (date day: 9)))

(test-equal
  "date+ month"
  #2020-10-10
  (date+ #2020-01-10
         (date month: 9)))

(test-equal
  "date+ day/month"
  #2020-10-10
  (date+ #2020-01-01
         (date day: 9 month: 9)))

(test-assert
  "date+ first literal"
  (date+ #2020-01-01
         (date day: 0)))

(test-assert
  "date+ second literal"
  (date+ #0001-01-01
         #0001-00-00))

(test-assert
  "date+ both literal"
  (date+ #2020-01-01
         #0000-00-00))

(test-equal
  "date+ year overflow"
  #2019-01-01
  (date+ #2018-12-31
         (date day: 1)))

(test-equal
  "date- year overflow"
  #2018-12-31
  (date- #2019-01-01
         (date day: 1)))

(test-equal
  "date- large"
  #0001-01-01
  (date- #2020-01-01
         #2019-00-00))

(test-equal
  "date- equal"
  (date year: -1 month: 11 day: 31)
  (date- #2020-01-01
         #2020-01-01))

(test-equal
    "Simple datetime construction"
  #2020-01-01T10:00:00
  (datetime
    date:
    #2020-01-01
    time:
    #10:00:00))

(test-equal
    "Datetime add date-only and time-only."
  #2020-01-01T10:00:00
  (datetime+
    (datetime
      date:
      #2020-01-01)
    (datetime
      time:
      #10:00:00)))

(test-equal
    "Datetime subtract time"
  #2020-10-09T14:00:00
  (datetime-
    #2020-10-10T00:00:00
    (datetime
      time:
      #10:00:00)))

(test-equal
    "Datetime subtract datetime"
  #2020-09-24T14:00:00
  (datetime-
    #2020-10-10T00:00:00
    #0000-00-15T10:00:00))

(test-equal
    "Date+ multiple"
  #2020-03-10
  (date+ #2020-03-01
         (date day: 4)
         (date day: 5)))

(let* ((diff overflow
             (time- #10:20:30
                    #10:20:30)))
  (test-equal
    "time- self"
    #00:00:00
    diff)
  (test-equal "time- self overflow" 0 overflow))

(let* ((diff overflow
             (time- #10:00:00
                    #10:00:01)))
  (test-equal
    "time- overflow 1s"
    #23:59:59
    diff)
  (test-equal
    "time- overflow 1s overflow"
    1
    overflow))

(let* ((diff overflow
             (time- #10:00:00
                    (time hour: (+ 48 4)))))
  (test-equal
    "time- overflow multiple"
    #06:00:00
    diff)
  (test-equal
    "time- overflow multiple overflow"
    2
    overflow))

(test-equal
  "datetime-difference self"
  #0000-00-00T00:00:00
  (datetime-difference
    (datetime
      date:
      #2020-01-01)
    (datetime
      date:
      #2020-01-01)))

;; NOTE
;; at the time of writing this returns #2020-02-00
;; The general question is, how is the last in a month handled?
;; TODO
(test-equal
    "Date+ over year end"
  #2020-01-31
  (date+ #2019-12-31
         (date month: 1)))


(test-assert "leap-year?" (leap-year? 2020))

(test-equal
  "Add to Leap day"
  #2020-02-29
  (date+ #2020-02-28
         (date day: 1)))

(test-equal
  "Parse ISO"
  #2021-12-30T13:53:33
  (string->datetime
    "2021-12-30T13:53:33"
    "~Y-~m-~dT~H:~M:~S"))

(test-equal
  "Parse ical date-time"
  #2021-12-30T13:53:33
  (string->datetime
    "20211230T135333"
    "~Y~m~dT~H~M~S"))

(test-equal
  "Parse single hour (padded)"
  (time hour: 5)
  (string->time "05" "~H"))

(test-equal
  "Parse single hour (non-padded)"
  (time hour: 5)
  (string->time "5" "~H"))

(test-equal
  "Parse month (swedish)"
  (date month: 5)
  (string->date
    "Maj"
    "~b"
    (make-locale (list LC_CTYPE LC_TIME) "sv_SE.UTF-8")))

(test-equal
  "Parse month (english)"
  (date month: 5)
  (string->date
    "May"
    "~b"
    (make-locale (list LC_CTYPE LC_TIME) "en_US.UTF-8")))

(test-equal
  "AM/PM AM"
  (time hour: 10)
  (string->time "10 AM" "~H ~p"))

(test-equal
  "AM/PM PM"
  (time hour: 22)
  (string->time "10 PM" "~H ~p"))

(test-equal
  "AM/PM AM 12"
  (time hour: 0)
  (string->time "12 AM" "~H ~p"))

(test-equal
  "AM/PM PM 12"
  (time hour: 12)
  (string->time "12 PM" "~H ~p"))

(test-equal
  "AM/PM PM (prefix)"
  (time hour: 22)
  (string->time "PM 10" "~p ~H"))

(test-equal
  "Parse complicated 1"
  #2021-12-30T10:56:00
  (string->datetime
    "Dec. 30, 2021, 10:56"
    "~b. ~d, ~Y, ~H:~M"
    (make-locale LC_TIME "en_US.UTF-8")))

(test-equal
  "Parse complicated 2"
  #2021-12-30T10:56:00
  (string->datetime
    "Dec. 30, 2021, 10:56 a.m."
    "~b. ~d, ~Y, ~H:~M"
    (make-locale LC_TIME "en_US.UTF-8")))

(test-equal
  "Parse complicated 3"
  #2021-12-30T22:56:00
  (string->datetime
    "Dec. 30, 2021, 10:56 p.m."
    "~b. ~d, ~Y, ~H:~M ~p"
    (make-locale LC_TIME "en_US.UTF-8")))

(test-equal
  "Parse date single digit day"
  (date day: 6)
  (string->date "6" "~d"))

(test-equal
  "Parse date single digit day, trailing comma"
  (date day: 6)
  (string->date "6," "~d,"))

(test-equal
  "Parse date single digit day, trailing comma + space"
  (date day: 6)
  (string->date "6, " "~d, "))

(define en_US
  (make-locale (list LC_CTYPE LC_TIME) "en_US.UTF-8"))

(define sv_SE
  (make-locale (list LC_CTYPE LC_TIME) "sv_SE.UTF-8"))

(test-equal "Week day name"
  "söndag" (week-day-name sun locale: sv_SE))
(test-equal "Week day name (modulo)"
  "söndag" (week-day-name (+ 7 sun) locale: sv_SE))

(test-equal "en month name - january"
  1 (parse-month "jan" en_US))

(test-equal "sv month name - januari"
  1 (parse-month "jan" sv_SE))

(test-equal "en month name - december"
  12 (parse-month "dec" en_US))

(test-equal "en month name - invalid"
  -1 (parse-month "inv" en_US))

(test-equal "sv month name - mAJ"
  5 (parse-month "mAJ" sv_SE))

(test-equal
  "Days in regular year"
  365
  (days-in-interval
    #2021-01-01
    #2021-12-31))

(test-equal
  "Days in leap year"
  366
  (days-in-interval
    #2020-01-01
    #2020-12-31))



(test-error "Construct invalid date (year)"
  'wrong-type-arg
  (date year: #f))

(test-error "Construct invalid date (month)"
  'wrong-type-arg
  (date month: #f))

(test-error "Construct invalid date (day)"
  'wrong-type-arg
  (date day: #f))


(test-assert "Current date is a date"
  (date? (current-date)))

(let ((t #20:30:40))
  (test-equal "As-time identity" t (as-time t)))

(let ((d #2020-10-05))
  (test-equal "As-date identity" d (as-date d)))

(test-equal "As-time date == 0"
  (time) (as-time (date)))
(test-equal "As-date time == 0"
  (date) (as-date (time)))

(test-error "As-time invalid argument"
  'wrong-type-arg
  (as-time #f))

(test-error "As-date invalid argument"
   'wrong-type-arg
  (as-date #f))

(test-error "As-datetime invalid argument"
  'wrong-type-arg
  (as-datetime #f))

(test-assert "Time-zero on empty time"
  (time-zero? (time)))

(test-error "Invalid month (below)"
  'out-of-range
  (days-in-month (date year: 2020 month: 0)))

(test-error "Invalid month (above)"
  'out-of-range
  (days-in-month (date year: 2020 month: 13)))

;; This both tests days-in-year for both cases, and leap year code for the weird years.
(test-equal "Leap year exception exception"
  366 (days-in-year (date year: 2000)))
(test-equal "Leap year exception"
  365 (days-in-year (date year: 1800)))

(test-equal "End of month leap year"
  #2020-02-29
  (end-of-month #2020-02-05))

(test-equal "Time-min"
  (time)
  (time-min (time) (time hour: 1)))

(test-equal "Time-max"
  (time hour: 1)
  (time-max (time) (time hour: 1)))

(test-equal "Date-min"
  (date)
  (date-min (date) (date year: 1)))

(test-equal "Date-max"
  (date year: 1)
  (date-max (date) (date year: 1)))

(test-equal "Datetime-min"
  (datetime)
  (datetime-min (datetime)
                (datetime hour: 1)))

(test-equal "Datetime-max"
  (datetime hour: 1)
  (datetime-max (datetime)
                (datetime hour: 1)))

;; month± mostly here for coverage,
;; actual tests are for date±
(test-equal "month+ dflt"
  (date month: 3 day: 1)
  (month+ (date month: 2 day: 1)))

(test-equal "month+ given change"
  (date month: 4 day: 1)
  (month+ (date month: 2 day: 1) 2))

(test-equal "month- dflt"
  (date month: 1 day: 1)
  (month- (date month: 2 day: 1)))

(test-equal "month- given change"
  (date month: 2 day: 1)
  (month- (date month: 4 day: 1) 2))

;; same for {add,remove}-day; mostly here for coverage.

(test-equal "add-day"
  (date month: 1 day: 2)
  (add-day (date month: 1 day: 1)))

(test-equal "remove-day"
  (date month: 1 day: 1)
  (remove-day (date month: 1 day: 2)))

;; TODO more week-number tests
(test-equal "Week 53"
  53 (week-number #2020-12-28 mon))

(test-equal "End of week"
  #2022-04-17 (end-of-week #2022-04-11 mon))
(test-equal "End of week (wednesday)"
  #2022-04-12 (end-of-week #2022-04-11 wed))

(define-values (pre mid post)
  (month-days #2020-03-01 mon))
(test-equal "month-days pre"
  (list #2020-02-24 #2020-02-25 #2020-02-26 #2020-02-27 #2020-02-28 #2020-02-29)
  pre)
(test-equal "month-days mid"
  (list #2020-03-01 #2020-03-02 #2020-03-03 #2020-03-04 #2020-03-05 #2020-03-06 #2020-03-07 #2020-03-08 #2020-03-09 #2020-03-10 #2020-03-11 #2020-03-12 #2020-03-13 #2020-03-14 #2020-03-15 #2020-03-16 #2020-03-17 #2020-03-18 #2020-03-19 #2020-03-20 #2020-03-21 #2020-03-22 #2020-03-23 #2020-03-24 #2020-03-25 #2020-03-26 #2020-03-27 #2020-03-28 #2020-03-29 #2020-03-30 #2020-03-31)
  mid)
(test-equal "month-days post"
  (list #2020-04-01 #2020-04-02 #2020-04-03 #2020-04-04 #2020-04-05)
  post)

(test-equal "Year day"
  32 (year-day #2020-02-01))

(test-equal "time->decimal-hour"
  10.5 (time->decimal-hour #10:30:00))

(test-equal "datetime->decimal-hour"
  34.5
  (datetime->decimal-hour
   (datetime day: 1 time: #10:30:00)))

(test-error "Datetime->decimal-hour fail on multi month"
  'misc-error
  (datetime->decimal-hour (datetime month: 1)))

(test-equal
    "Datetime->decimal hour suceed on multi month"
    (exact->inexact (* 24 28))
  (datetime->decimal-hour (datetime month: 1)
                          #2022-02-01))




;;; Commentary:
;; Tests timespan overlaps and month-streams.
;; Separate from tests/datetime.scm since
;; (datetime util) originally was its own module.
;;; Code:


(test-assert
  "jan->dec"
  (stream->list
    (stream-take
      11
      (month-stream
        #2020-01-01))))

(test-assert
  "dec->jan"
  (stream->list
    (stream-take
      2
      (month-stream
        #2020-12-01))))

(test-assert
  "dec->feb"
  (stream->list
    (stream-take
      3
      (month-stream
        #2020-12-01))))

(test-assert
  "20 months"
  (stream->list
    (stream-take
      20
      (month-stream
        #2020-01-01))))

(test-equal
  "Correct months"
  (list #2020-02-01
        #2020-03-01
        #2020-04-01
        #2020-05-01
        #2020-06-01
        #2020-07-01
        #2020-08-01
        #2020-09-01
        #2020-10-01
        #2020-11-01
        #2020-12-01
        #2021-01-01)
  (stream->list
    (stream-take
      12
      (month-stream
        #2020-02-01))))

(test-assert
  "in-date-range?"
  (not ((in-date-range?
          #2020-01-01
          #2020-02-29)
        #2018-02-02)))

(test-assert
  "A"
  (timespan-overlaps?
    #2020-01-01
    #2020-01-10
    #2020-01-05
    #2020-01-15))

(test-assert
  "A, shared start"
  (timespan-overlaps?
    #2020-01-01
    #2020-01-10
    #2020-01-01
    #2020-01-15))

(test-assert
  "A, tangential"
  (not (timespan-overlaps?
         #2020-01-01T00:00:00
         #2020-01-10T00:00:00
         #2020-01-10T00:00:00
         #2020-01-30T00:00:00)))

(test-assert
  "s1 instant"
  (timespan-overlaps?
    #2020-01-15T10:00:00
    #2020-01-15T10:00:00
    #2020-01-10T00:00:00
    #2020-01-30T00:00:00))

(test-assert
  "s2 instant"
  (timespan-overlaps?
    #2020-01-10T00:00:00
    #2020-01-30T00:00:00
    #2020-01-15T10:00:00
    #2020-01-15T10:00:00))

(test-assert
  "s1 instant, shared start with s2"
  (timespan-overlaps?
    #2020-01-15T10:00:00
    #2020-01-15T10:00:00
    #2020-01-15T10:00:00
    #2020-01-30T00:00:00))

(test-assert
  "s1 instant, shared end with s2"
  (not (timespan-overlaps?
         #2020-01-15T10:00:00
         #2020-01-15T10:00:00
         #2020-01-10T00:00:00
         #2020-01-15T10:00:00)))

(test-assert
  "s2 instant, shared start with s1"
  (timespan-overlaps?
    #2020-01-15T10:00:00
    #2020-01-30T00:00:00
    #2020-01-15T10:00:00
    #2020-01-15T10:00:00))

(test-assert
  "s2 instant, shared end with s1"
  (not (timespan-overlaps?
         #2020-01-10T00:00:00
         #2020-01-15T10:00:00
         #2020-01-15T10:00:00
         #2020-01-15T10:00:00)))

(test-assert
  "both instant"
  (not (timespan-overlaps?
         #2020-01-15T10:00:00
         #2020-01-15T10:00:00
         #2020-01-15T10:00:00
         #2020-01-15T10:00:00)))

(test-assert
  "tangential whole day"
  (not (timespan-overlaps?
         #2020-01-01
         #2020-01-02
         #2020-01-02
         #2020-01-03)))

(test-assert
  "B"
  (timespan-overlaps?
    #2020-01-05
    #2020-01-15
    #2020-01-01
    #2020-01-10))

(test-assert
  "E"
  (timespan-overlaps?
    #2020-01-01
    #2020-01-10
    #2020-01-01
    #2020-01-10))




;;; Commentary:
;; Tests that all ordering predicates for dates,
;; times, and datetimes hold.
;;; Code:

(test-assert "date< empty" (date<))

(test-assert
  "date< single"
  (date< #2020-01-10))

(test-assert
  "date< double"
  (date< #2020-01-10
         #2020-01-11))

(test-assert
  "date< tripple"
  (date< #2020-01-10
         #2020-01-11
         #2020-01-12))

(test-assert
  "date< tripple negate"
  (not (date< #2020-01-10
              #2020-01-12
              #2020-01-11)))

(test-assert "date<= empty" (date<=))

(test-assert
  "date<= single"
  (date<= #2020-01-10))

(test-assert
  "date<= double"
  (date<=
    #2020-01-10
    #2020-01-11))

(test-assert
  "date<="
  (not (date<=
         #2020-01-01
         #2018-05-15
         #2020-01-31)))

(test-assert
  "date<= equal"
  (date<=
    #2018-05-15
    #2018-05-15))

(test-assert
  "date<"
  (not (date< #2020-01-01
              #2018-05-15
              #2020-01-31)))

(test-assert
  "date>"
  (not (date> #2020-01-31
              #2018-05-15
              #2020-01-01)))

(test-assert
  "date>="
  (not (date>=
         #2020-01-31
         #2018-05-15
         #2020-01-01)))

(test-assert
  "time< simple"
  (time< #05:00:00
         #10:00:00))

(test-assert
  "time<"
  (time< (time)
         #10:00:00))

(test-assert
  "date/-time<"
  (date/-time<
    #2020-01-01
    #2020-01-02))

(test-assert
  "not date/-time<"
  (not (date/-time<
         #2020-01-01
         #2020-01-01)))

(test-assert
  "date/-time< only other dt"
  (date/-time<
    #2020-01-01
    #2020-01-02T10:00:00))

(test-assert
  "date/-time< other dt, same date"
  (date/-time<
    #2020-01-01
    #2020-01-01T10:00:00))

;; In UTC+2 (CEST) the below datetime overflows into midnight the following
;; day. Earlier versions of this program only looked at the time component
(test-assert
  "date/-time< TZ overflow"
  (date/-time<
    #2020-04-05
    (datetime
      date:
      #2020-04-05
      time:
      #22:00:00
      tz:
      "UTC")))

(test-assert
  "date/-time< time-only"
  (date/-time<
    #00:00:00
    #10:00:00))

(test-assert
  (not (date/-time<
         #2018-11-30T08:10:00
         #2014-04-13T16:00:00)))



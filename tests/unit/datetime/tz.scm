;;; Commentary:
;; Tests that datetime->unix-time correctly converts between Olssen
;; timezone definitions (e.g. Europe/Stockholm), into correct times
;; and offsets (in unix time).
;; Also indirectly tests the Zone Info Compiler (datetime zic), since
;; the zoneinfo comes from there.
;;; Code:

(define-module (test tz)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((datetime)
               :select (parse-ics-datetime
                        datetime
                        date
                        time
                        datetime->unix-time
                        unix-time->datetime
                        get-datetime))
  :use-module ((hnh util env) :select (let-env)))

;; London alternates between +0000 and +0100
(let-env
  ((TZ "Europe/London"))
  (test-equal
    "London winter"
    (datetime year: 2020 month: 01 day: 12 hour: 13 minute: 30 second: 00)
    (get-datetime
      (parse-ics-datetime "20200112T133000Z")))
  (test-equal
    "London summer"
    (datetime year: 2020 month: 06 day: 12 hour: 14 minute: 30 second: 00)
    (get-datetime
      (parse-ics-datetime "20200612T133000Z"))))

;; Stockholm alternates between +0100 and +0200
(let-env
  ((TZ "Europe/Stockholm"))
  (test-equal
    "Stockholm winter"
    (datetime year: 2020 month: 01 day: 12 hour: 14 minute: 30 second: 00)
    (get-datetime
      (parse-ics-datetime "20200112T133000Z")))
  (test-equal
    "Stockholm summer"
    (datetime year: 2020 month: 06 day: 12 hour: 15 minute: 30 second: 00)
    (get-datetime
      (parse-ics-datetime "20200612T133000Z"))))

(test-equal
  -10800
  (datetime->unix-time
    (parse-ics-datetime
      "19700101T000000"
      "Europe/Tallinn")))

(test-equal
  -3600
  (datetime->unix-time
    (parse-ics-datetime
      "19700101T000000"
      "Europe/Stockholm")))

(test-equal
  0
  (datetime->unix-time
    (parse-ics-datetime "19700101T000000Z")))

;; yes, really
(test-equal
  -3600
  (datetime->unix-time
    (parse-ics-datetime
      "19700101T000000"
      "Europe/London")))

(test-equal
  (datetime
    date:
    (date year: 1970 month: 01 day: 01)
    time:
    (time hour: 00 minute: 00 second: 00)
    tz:
    "UTC")
  (unix-time->datetime 0))


'((datetime))

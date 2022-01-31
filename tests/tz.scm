;;; Commentary:
;; Tests that datetime->unix-time correctly converts between Olssen
;; timezone definitions (e.g. Europe/Stockholm), into correct times
;; and offsets (in unix time).
;; Also indirectly tests the Zone Info Compiler (datetime zic), since
;; the zoneinfo comes from there.
;;; Code:

(((datetime)
  parse-ics-datetime
  datetime date time
  datetime->unix-time
  unix-time->datetime
  get-datetime)
 ((hnh util) let-env))

;; London alternates between +0000 and +0100
(let-env ((TZ "Europe/London"))
 (test-equal "London winter"
   #2020-01-12T13:30:00
   (get-datetime (parse-ics-datetime "20200112T133000Z")))
 (test-equal "London summer"
   #2020-06-12T14:30:00
   (get-datetime (parse-ics-datetime "20200612T133000Z"))))

;; Stockholm alternates between +0100 and +0200
(let-env ((TZ "Europe/Stockholm"))
 (test-equal "Stockholm winter"
   #2020-01-12T14:30:00
   (get-datetime (parse-ics-datetime "20200112T133000Z")))
 (test-equal "Stockholm summer"
   #2020-06-12T15:30:00
   (get-datetime (parse-ics-datetime "20200612T133000Z"))) )

(test-equal
    -10800
  (datetime->unix-time
   (parse-ics-datetime "19700101T000000" "Europe/Tallinn")))

(test-equal
    -3600
  (datetime->unix-time
   (parse-ics-datetime "19700101T000000" "Europe/Stockholm")))

(test-equal
    0
  (datetime->unix-time (parse-ics-datetime "19700101T000000Z")))

;; yes, really
(test-equal
    -3600
  (datetime->unix-time
   (parse-ics-datetime "19700101T000000" "Europe/London")))

(test-equal
    #1970-01-01T00:00:00Z
  (unix-time->datetime 0))

;;; Commentary:
;; Tests date, time, and datetime creation,
;; (output) formatting, and arithmetic.
;;; Code:

(((datetime) date+ date-
  time+ time-
  year month day
  date time
  datetime
  datetime+
  datetime<=?
  datetime-difference
  datetime-
  leap-year?
  string->date string->time string->datetime
  parse-month
  )
 ((ice-9 format) format)
 ((hnh util) let*)
 ((ice-9 i18n) make-locale)
 ((guile) LC_TIME)
 )

(test-equal "empty time"
  (time) #00:00:00)

(test-assert "Synatx date"
  #2020-01-01)

(test-assert "Test year type"
  (integer? (year (date year: 2020))))

(test-assert "Test mmnth type"
  (integer? (month (date month: 1))))

(test-assert "Test day type"
  (integer? (day (date day: 1))))

(test-equal "Manual print (any)"
  "2020-10-10"
  (let ((d (date year: 2020 month: 10 day: 10)))
    (format #f "~a-~a-~a"
            (year d) (month d) (day d))))

(test-equal "Manual print (number)"
  "2020-10-10"
  (let ((d (date year: 2020 month: 10 day: 10)))
    (format #f "~d-~d-~d"
            (year d) (month d) (day d))))

(test-equal "Date print"
  "#2020-01-01"
  (format #f "~a" (date year: 2020 month: 1 day: 1)))

(test-equal "Syntax date="
  (date year: 2020 month: 1 day: 1)
  #2020-01-01)

(test-equal "Syntax time="
  (time hour: 13 minute: 37 second: 0)
  #13:37:00)

(test-equal "Syntax Datetime="
  (datetime year: 2020 month: 1 day: 1 hour: 13 minute: 37 second: 0)
  #2020-01-01T13:37:00)

(test-equal #2020-02-28 (date- #2020-03-05 (date day: 6)))
(test-equal #2020-02-29 (date- #2020-03-05 (date day: 5)))
(test-equal #2020-03-01 (date- #2020-03-05 (date day: 4)))

(test-equal "date+ day" #2020-10-10 (date+ #2020-10-01 (date day: 9)))
(test-equal "date+ month" #2020-10-10 (date+ #2020-01-10 (date month: 9)))
(test-equal "date+ day/month" #2020-10-10 (date+ #2020-01-01 (date day: 9 month: 9)))
;; (test-equal "date+ year" #4040-10-10 (date+ #2020-10-10 (date year: 2020)))

(test-assert "date+ first literal" (date+ #2020-01-01 (date day: 0)))
(test-assert "date+ second literal" (date+ (date year: 1 month: 1 day: 1) #0001-00-00))
(test-assert "date+ both literal" (date+ #2020-01-01 #0000-00-00))

(test-equal "date+ year overflow" #2019-01-01 (date+ #2018-12-31 (date day: 1)))
(test-equal "date- year overflow" #2018-12-31 (date- #2019-01-01 (date day: 1)))

;; (test-equal "date+ large" #4040-10-10 (date+ #2020-05-03 #2020-05-07))

(test-equal "date- large" #0001-01-01 (date- #2020-01-01 #2019-00-00))

;; Datum är spännande
(test-equal "date- equal" (date year: -1 month: 11 day: 31)
            (date- #2020-01-01 #2020-01-01))

(test-equal #2020-01-01T10:00:00 (datetime date: #2020-01-01
                                           time: #10:00:00))
(test-equal #2020-01-01T10:00:00
  (datetime+ (datetime date: #2020-01-01)
             (datetime time: #10:00:00)))

(test-equal
    #2020-10-09T14:00:00
  (datetime- #2020-10-10T00:00:00
             (datetime time: #10:00:00)))

(test-equal
    #2020-09-24T14:00:00
  (datetime- #2020-10-10T00:00:00
             #0000-00-15T10:00:00))


(test-equal #2020-03-10
  (date+ #2020-03-01
         (date day: 4)
         (date day: 5)))


(let* ((diff overflow (time- #10:20:30 #10:20:30)))
  (test-equal "time- self" #00:00:00 diff)
  (test-equal "time- self overflow" 0 overflow))

(let* ((diff overflow (time- #10:00:00 #10:00:01)))
  (test-equal "time- overflow 1s" #23:59:59 diff)
  (test-equal "time- overflow 1s overflow" 1 overflow))


(let* ((diff overflow (time- #10:00:00 (time hour: (+ 48 4)))))
  (test-equal "time- overflow multiple" #06:00:00 diff)
  (test-equal "time- overflow multiple overflow" 2 overflow))

(test-equal "datetime-difference self"
  #0000-00-00T00:00:00
  (datetime-difference (datetime date: #2020-01-01) (datetime date: #2020-01-01)))

;; (test-assert
;;     (datetime- #2018-01-17T10:00:00
;;                #2018-01-17T08:00:00))


;; (test-assert
;;     (datetime<=? (datetime time: (time hour: 24))
;;                  (datetime- #2018-01-17T10:00:00
;;                             #2018-01-17T08:00:00)))


;; NOTE
;; at the time of writing this returns #2020-02-00
;; The general question is, how is the last in a month handled?
(test-equal
    #2020-01-31
  (date+ #2019-12-31 (date month: 1)))

(test-assert (leap-year? 2020))

(test-equal "Add to Leap day"
    #2020-02-29 (date+ #2020-02-28 (date day: 1)))


(test-equal "Parse ISO"
  #2021-12-30T13:53:33
  (string->datetime "2021-12-30T13:53:33" "~Y-~m-~dT~H:~M:~S"))

(test-equal "Parse ical date-time"
  #2021-12-30T13:53:33
  (string->datetime "20211230T135333" "~Y~m~dT~H~M~S"))


(test-equal "Parse single hour (padded)"
            (time hour: 5)
            (string->time "05" "~H"))

(test-equal "Parse single hour (non-padded)"
            (time hour: 5)
            (string->time "5" "~H"))

(test-equal "Parse month (swedish)"
            (date month: 5)
            (string->date "Maj" "~b" (make-locale LC_TIME "sv_SE.UTF-8")))

(test-equal "Parse month (english)"
            (date month: 5)
            (string->date "May" "~b" (make-locale LC_TIME "en_US.UTF-8")))

(test-equal "AM/PM AM"
            (time hour: 10)
            (string->time "10 AM" "~H ~p"))

(test-equal "AM/PM PM"
            (time hour: 22)
            (string->time "10 PM" "~H ~p"))

(test-equal "AM/PM AM 12"
            (time hour: 0)
            (string->time "12 AM" "~H ~p"))

(test-equal "AM/PM PM 12"
            (time hour: 12)
            (string->time "12 PM" "~H ~p"))

(test-equal "AM/PM PM (prefix)"
            (time hour: 22)
            (string->time "PM 10" "~p ~H"))

(test-equal "Parse complicated 1"
            #2021-12-30T10:56:00
            (string->datetime "Dec. 30, 2021, 10:56"
                              "~b. ~d, ~Y, ~H:~M"
                              (make-locale LC_TIME "en_US.UTF-8")))

(test-equal "Parse complicated 2"
            #2021-12-30T10:56:00
            (string->datetime "Dec. 30, 2021, 10:56 a.m."
                              "~b. ~d, ~Y, ~H:~M"
                              (make-locale LC_TIME "en_US.UTF-8")))

(test-equal "Parse complicated 3"
            #2021-12-30T22:56:00
            (string->datetime "Dec. 30, 2021, 10:56 p.m."
                              "~b. ~d, ~Y, ~H:~M ~p"
                              (make-locale LC_TIME "en_US.UTF-8")))

(test-equal "Parse date single digit day"
            (date day: 6)
            (string->date "6" "~d"))

(test-equal "Parse date single digit day, trailing comma"
            (date day: 6)
            (string->date "6," "~d,"))

(test-equal "Parse date single digit day, trailing comma + space"
            (date day: 6)
            (string->date "6, " "~d, "))


(define en_US (make-locale LC_TIME "en_US.UTF-8"))
(define sv_SE (make-locale LC_TIME "sv_SE.UTF-8"))

(test-equal 1 (parse-month "jan" en_US))
(test-equal 1 (parse-month "jan" sv_SE))

(test-equal 12 (parse-month "dec" en_US))
(test-equal -1 (parse-month "inv" en_US))

(test-equal 5 (parse-month "mAJ" sv_SE))

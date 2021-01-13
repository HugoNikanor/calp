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
  )
 ((ice-9 format) format)
 ((calp util) let*)
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



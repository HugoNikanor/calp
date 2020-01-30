(((srfi srfi-19 alt) date+ date-
  year month day
  date time
  date<
  datetime
  datetime+
  datetime-
  datetime<=?
  )
 ((ice-9 format) format)
 )

(test-assert "Synatx date"
  #2020-01-01)

(test-assert "Test year type"
  (integer? (year (date year: 2020))))

(test-assert "Test month type"
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
  "2020-01-01"
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


(test-equal #2020-03-10
  (date+ #2020-03-01
         (date day: 4)
         (date day: 5)))


(test-assert "date< empty"
  (date<))

(test-assert "date< single"
  (date< #2020-01-10))

(test-assert "date< double"
  (date< #2020-01-10 #2020-01-11))

(test-assert "date< tripple"
  (date< #2020-01-10 #2020-01-11 #2020-01-12))

(test-assert "date< tripple negate"
  (not (date< #2020-01-10 #2020-01-12 #2020-01-11)))

(test-assert
    (datetime- #2018-01-17T10:00:00
               #2018-01-17T08:00:00))


(test-assert
    (datetime<=? (datetime time: (time hour: 24))
                 (datetime- #2018-01-17T10:00:00
                            #2018-01-17T08:00:00)))

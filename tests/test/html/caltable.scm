(define-module (test html caltable)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (calp html caltable)
  :use-module (datetime)
  ;; causes translated parts of the generated document to work
  :use-module (calp translation)
  )

;; Not the most robust test, but at least it shows us when we break something
(test-equal "Whole fucking caltable"
  `(div (@ (class "small-calendar"))
        (div (@ (class "column-head row-head")) ,(G_ "v."))
        (div (@ (class "column-head")) "Må")
        (div (@ (class "column-head")) "Ti")
        (div (@ (class "column-head")) "On")
        (div (@ (class "column-head")) "To")
        (div (@ (class "column-head")) "Fr")
        (div (@ (class "column-head")) "Lö")
        (div (@ (class "column-head")) "Sö")
        (div (@ (class "row-head")) 13)
        (div (@ (class "row-head")) 14)
        (div (@ (class "row-head")) 15)
        (div (@ (class "row-head")) 16)
        (div (@ (class "row-head")) 17)
        (a (@ (class "prev")
              (href "2022-03-01.html" "#" "2022-03-28"))
           (time (@ (datetime "2022-03-28")) 28))
        (a (@ (class "prev")
              (href "2022-03-01.html" "#" "2022-03-29"))
           (time (@ (datetime "2022-03-29")) 29))
        (a (@ (class "prev")
              (href "2022-03-01.html" "#" "2022-03-30"))
           (time (@ (datetime "2022-03-30")) 30))
        (a (@ (class "prev")
              (href "2022-03-01.html" "#" "2022-03-31"))
           (time (@ (datetime "2022-03-31")) 31))
        (a (@ (href "#" "2022-04-01"))
           (time (@ (datetime "2022-04-01")) 1))
        (a (@ (href "#" "2022-04-02"))
           (time (@ (datetime "2022-04-02")) 2))
        (a (@ (href "#" "2022-04-03"))
           (time (@ (datetime "2022-04-03")) 3))
        (a (@ (href "#" "2022-04-04"))
           (time (@ (datetime "2022-04-04")) 4))
        (a (@ (href "#" "2022-04-05"))
           (time (@ (datetime "2022-04-05")) 5))
        (a (@ (href "#" "2022-04-06"))
           (time (@ (datetime "2022-04-06")) 6))
        (a (@ (href "#" "2022-04-07"))
           (time (@ (datetime "2022-04-07")) 7))
        (a (@ (href "#" "2022-04-08"))
           (time (@ (datetime "2022-04-08")) 8))
        (a (@ (href "#" "2022-04-09"))
           (time (@ (datetime "2022-04-09")) 9))
        (a (@ (href "#" "2022-04-10"))
           (time (@ (datetime "2022-04-10")) 10))
        (a (@ (href "#" "2022-04-11"))
           (time (@ (datetime "2022-04-11")) 11))
        (a (@ (href "#" "2022-04-12"))
           (time (@ (datetime "2022-04-12")) 12))
        (a (@ (href "#" "2022-04-13"))
           (time (@ (datetime "2022-04-13")) 13))
        (a (@ (href "#" "2022-04-14"))
           (time (@ (datetime "2022-04-14")) 14))
        (a (@ (href "#" "2022-04-15"))
           (time (@ (datetime "2022-04-15")) 15))
        (a (@ (href "#" "2022-04-16"))
           (time (@ (datetime "2022-04-16")) 16))
        (a (@ (href "#" "2022-04-17"))
           (time (@ (datetime "2022-04-17")) 17))
        (a (@ (href "#" "2022-04-18"))
           (time (@ (datetime "2022-04-18")) 18))
        (a (@ (href "#" "2022-04-19"))
           (time (@ (datetime "2022-04-19")) 19))
        (a (@ (href "#" "2022-04-20"))
           (time (@ (datetime "2022-04-20")) 20))
        (a (@ (href "#" "2022-04-21"))
           (time (@ (datetime "2022-04-21")) 21))
        (a (@ (href "#" "2022-04-22"))
           (time (@ (datetime "2022-04-22")) 22))
        (a (@ (href "#" "2022-04-23"))
           (time (@ (datetime "2022-04-23")) 23))
        (a (@ (href "#" "2022-04-24"))
           (time (@ (datetime "2022-04-24")) 24))
        (a (@ (href "#" "2022-04-25"))
           (time (@ (datetime "2022-04-25")) 25))
        (a (@ (href "#" "2022-04-26"))
           (time (@ (datetime "2022-04-26")) 26))
        (a (@ (href "#" "2022-04-27"))
           (time (@ (datetime "2022-04-27")) 27))
        (a (@ (href "#" "2022-04-28"))
           (time (@ (datetime "2022-04-28")) 28))
        (a (@ (href "#" "2022-04-29"))
           (time (@ (datetime "2022-04-29")) 29))
        (a (@ (href "#" "2022-04-30"))
           (time (@ (datetime "2022-04-30")) 30))
        (a (@ (class "next")
              (href "2022-05-01.html" "#" "2022-05-01"))
           (time (@ (datetime "2022-05-01")) 1)))

  (parameterize ((week-start mon))
    (cal-table start-date: #2022-04-01
               end-date: #2022-04-30
               next-start: (lambda (d) (date+ d (date month: 1)))
               prev-start: (lambda (d) (date- d (date month: 1))))))


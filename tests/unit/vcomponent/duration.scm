(define-module (test duration)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (vcomponent type duration))

;;; Tests extracted from RFC5545 through the following script
;; #!/bin/sh
;; dur_day="[0-9]+D"
;; dur_second="[0-9]+S"
;; dur_minute="[0-9]+M($dur_second)?"
;; dur_hour="[0-9]+H($dur_minute)?"
;; dur_week="[0-9]+W"
;; dur_time="T($dur_hour|$dur_minute|$dur_second)"
;; dur_date="$dur_day($dur_time)?"
;; dur_value="[+-]?P($dur_date|$dur_time|$dur_week)"
;; grep -o -E "$dur_value" ~/.local/doc/rfc/rfc5545.txt


(test-group "Parse duration"
 (test-equal (duration day: 15
                       time: (time hour: 5 second: 20))
   (string->duration "P15DT5H0M20S"))
 (test-equal (duration sign: '- day: 2) (string->duration "-P2D"))
 (test-equal (duration week: 7) (string->duration "P7W"))
 (test-equal (duration sign: '- time: (time minute: 15))
   (string->duration "-PT15M"))
 (test-equal (duration time: (time minute: 15))
   (string->duration "PT15M"))
 (test-equal (duration time: (time hour: 1))
   (string->duration "PT1H"))
 (test-equal (duration time: (time hour: 1))
   (string->duration "PT1H0M0S"))
 (test-equal (duration sign: '- time: (time minute: 30))
   (string->duration "-PT30M"))
 (test-equal (duration time: (time hour: 3))
   (string->duration "PT3H"))
 (test-equal (duration time: (time hour: 5 minute: 30))
   (string->duration "PT5H30M"))
 (test-equal (duration time: (time minute: 5))
   (string->duration "PT5M"))
 (test-equal (duration time: (time hour: 6 minute: 30))
   (string->duration "PT6H30M"))
 (test-equal (duration time: (time hour: 8 minute: 30))
   (string->duration "PT8H30M")))

(test-group "Format duration"
  (test-equal "P15DT5H1M20S"
    (duration->string (duration
                      day: 15
                      time: (time hour: 5 minute: 1 second: 20))))
  (test-equal "-P7W"
    (duration->string (duration week: 7 sign: '-))))

(test-error "Failure to construct invalid duration"
  'misc-error
  (duration week: 1 day: 1))

(test-error "Completely wrong duration"
  'parse-error
  (string->duration "Something weird"))

(test-error "Duration with extra fluff at end"
  'warning
  (string->duration "-P7WH"))



'((vcomponent type duration))

(define-module (test vcomponent-formats-common-types)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent formats common types)
               :select (get-parser))
  :use-module ((datetime) :select (date time datetime)))



(define parse-binary (get-parser 'BINARY))
;; TODO



(define parse-boolean (get-parser 'BOOLEAN))

(test-equal #t (parse-boolean #f "TRUE"))
(test-equal #f (parse-boolean #f "FALSE"))

(test-error 'warning (parse-boolean #f "ANYTHING ELSE"))



(define parse-cal-address
  (get-parser 'CAL-ADDRESS))

(test-equal "Test uri is passthrough"
  74 (parse-cal-address #f 74))



(define parse-date (get-parser 'DATE))

(test-equal
  (date year: 2021 month: 12 day: 02)
  (parse-date #f "20211202"))
;; TODO negative test here

(define parse-datetime (get-parser 'DATE-TIME))

(test-equal
    (datetime year: 2021 month: 12 day: 02 hour: 10 minute: 20 second: 30)
  (parse-datetime
    (make-hash-table)
    "20211202T102030"))

;; TODO tests with timezones here
;; TODO test -X-HNH-ORIGINAL here

;; TODO negative test here



(define parse-duration (get-parser 'DURATION))

;; assume someone else tests this one
;; (test-eq (@ (vcomponent duration) parse-duration)
;;   parse-duration)



(define parse-float (get-parser 'FLOAT))

(test-equal 1.0 (parse-float #f "1.0"))
(test-equal 1 (parse-float #f "1"))
(test-equal 1/2 (parse-float #f "1/2"))

;; TODO negative test here?



(define parse-integer (get-parser 'INTEGER))

(test-equal
  "parse integer"
  123456
  (parse-integer #f "123456"))

(test-equal
  "parse bigint"
  123451234512345123456666123456
  (parse-integer
    #f
    "123451234512345123456666123456"))

;; TODO is this expected behaivour?
(test-error 'warning (parse-integer #f "failure"))

(test-error
  "Non-integers aren't integers"
  'warning
  (parse-integer #f "1.1"))

(test-equal
  "But exact floats are"
  1.0
  (parse-integer #f "1.0"))



(define parse-period (get-parser 'PERIOD))

;; TODO



(define parse-recur (get-parser 'RECUR))

;; (test-eq (@ (vcomponent recurrence parse) parse-recurrence-rule))



(define parse-text (get-parser 'TEXT))

;; TODO



(define parse-time (get-parser 'TIME))

(test-equal
  (time hour: 10 minute: 20 second: 30)
  (parse-time #f "102030"))
;; TODO negative test here



(define parse-uri (get-parser 'URI))

(test-equal "Test uri is passthrough" 74 (parse-uri #f 74))



(define parse-utc-offset
  (get-parser 'UTC-OFFSET))

;; TODO

'((vcomponent formats common types))

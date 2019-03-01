(define-module (srfi srfi-19 setters) 
  #:use-module (srfi srfi-19)           ; Date/Time
  ;; (record-type-fields (@@ (srfi srfi-19) date))
  #:export (nanosecond second minute hour day month year zone-offset))


(define nanosecond (make-procedure-with-setter (@ (srfi srfi-19) date-nanosecond) (@@ (srfi srfi-19) set-date-nanosecond!)))
(define second (make-procedure-with-setter (@ (srfi srfi-19) date-second) (@@ (srfi srfi-19) set-date-second!)))
(define minute (make-procedure-with-setter (@ (srfi srfi-19) date-minute) (@@ (srfi srfi-19) set-date-minute!)))
(define hour (make-procedure-with-setter (@ (srfi srfi-19) date-hour) (@@ (srfi srfi-19) set-date-hour!)))
(define day (make-procedure-with-setter (@ (srfi srfi-19) date-day) (@@ (srfi srfi-19) set-date-day!)))
(define month (make-procedure-with-setter (@ (srfi srfi-19) date-month) (@@ (srfi srfi-19) set-date-month!)))
(define year (make-procedure-with-setter (@ (srfi srfi-19) date-year) (@@ (srfi srfi-19) set-date-year!)))
(define zone-offset (make-procedure-with-setter (@ (srfi srfi-19) date-zone-offset) (@@ (srfi srfi-19) set-date-zone-offset!)))


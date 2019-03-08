(define-module (srfi srfi-19 util)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 setters)
  #:export (copy-date
            drop-time! drop-time
            today?
            seconds minutes hours days weeks 
            time-add
            time->string))

#;
(define (copy-date date)
  "Returns a copy of the given date structure"
  (let* ((date-type (@@ (srfi srfi-19) date))
         (access (lambda (field) ((record-accessor date-type field) date))))
    (apply make-date (map access (record-type-fields date-type)))))

(define (drop-time! date)
  "Sets the hour, minute, second and nanosecond attribute of date to 0."
  (set! (hour date) 0)
  (set! (minute date) 0)
  (set! (second date) 0)
  (set! (nanosecond date) 0)
  date)

(define (drop-time date)
  "Returns a copy of date; with the hour, minute, second and nanosecond
attribute set to 0."
  (set-fields date
              ((date-hour) 0)
              ((date-minute) 0)
              ((date-second) 0)
              ((date-nanosecond) 0)))


(define seconds  1)
(define minutes 60)
(define hours   (* 60 minutes))
(define days    (* 24 hours))
(define weeks   (* 7 days))

(define (time-add time amount unit)
  (add-duration time (make-time time-duration 0 (* amount unit))))

(define (today? time)
  (let* ((now (date->time-utc (current-date)))
         (then (time-add now 1 days)))
    (and (time<=? now time)
         (time<=? time then))))

(define* (time->string time #:optional (format "~c"))
  (date->string (time-utc->date time) format))

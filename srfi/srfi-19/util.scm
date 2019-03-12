(define-module (srfi srfi-19 util)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 setters)
  #:export (copy-date
            drop-time! drop-time
            in-day? today?
            ;; seconds minutes hours days weeks
            ;; time-add
            make-duration
            time->string
            add-day remove-day))

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
attribute set to 0. Can also be seen as \"Start of day\""
  (set-fields date
              ((date-hour) 0)
              ((date-minute) 0)
              ((date-second) 0)
              ((date-nanosecond) 0)))

(define (make-duration s)
  (make-time time-duration 0 s))

(define (in-day? day-date time)
  (let* ((now (date->time-utc (drop-time day-date)))
         (then (add-duration now (make-duration (* 60 60 24)))))
    (and (time<=? now time)
         (time<=? time then))))

(define (today? time)
  (in-day? (current-date) time))

(define* (time->string time #:optional (format "~c"))
  (date->string (time-utc->date time) format))


(define (add-day time)
  (add-duration time (make-time time-duration 0 (* 60 60 24))))

(define (remove-day time)
  (add-duration time (make-time time-duration 0 (- (* 60 60 24)))))

(define-module (srfi srfi-19 util)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 setters)
  #:export (copy-date
            drop-time! drop-time
            localize-date
            date-today?
            seconds minutes hours days weeks 
            date-add
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
  #; 
  (let ((new-date (copy-date date)))    ;
  (drop-time! new-date))
  (set-fields date
              ((date-hour) 0)
              ((date-minute) 0)
              ((date-second) 0)
              ((date-nanosecond) 0)))


#;
(define (%date<=? a b)
  (time<=? (date->time-utc a)
           (date->time-utc b)))

#;
(define (localize-date date)
  "Returns a <date> object representing the same datetime as `date`, but
transposed to the current timezone. Current timezone gotten from
(current-date)."
  (time-utc->date (date->time-utc date)
                  (date-zone-offset (current-date))))

(define (today? time)
  (let* ((now (current-date))
         (then (add-duration time (make-time time-difference 0 (* 24 3600)))))
    (and (time<=? time now)
         (time<=? now then))))

  #;
(define (date-today? input-date)
  (let* ((date (current-date))
         (now (drop-time date))
         (then (copy-date now)))
    (set! (day then)
          (1+ (day then)))
    (and (%date<=? now input-date)
         (%date<=? input-date then))))

(define seconds  1)
(define minutes 60)
(define hours   (* 60 minutes))
(define days    (* 24 hours))
(define weeks   (* 7 days))

(define (time-add time amount unit)
  (add-duration time (make-time time-duration 0 (* amount unit))))

#;
(define (date-add date amount unit)
  (time-utc->date (add-duration (date->time-utc date)
                 (make-time time-duration 0 (* amount unit)))))

(define* (time->string time #:optional (format "~c"))
  (date->string (time-utc->date time) format))

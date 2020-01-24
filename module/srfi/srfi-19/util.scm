(define-module (srfi srfi-19 util)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 setters)
  #:use-module (srfi srfi-41)
  #:use-module (util)
  #:export (copy-date
            drop-time! drop-time
            in-day? today?
            ;; seconds minutes hours days weeks
            ;; time-add
            make-duration
            time->string
            add-day remove-day
            date))

(set-record-type-printer!
 (@@ (srfi srfi-19) date)
 (lambda (r p) (display (date->string r "~Y-~m-~d ~H:~M:~S~z")
                   p)))

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
  (set-> date
         (date-hour 0)
         (date-minute 0)
         (date-second 0)
         (date-nanosecond 0)))


(define-public (start-of-month date)
  (set-fields date ((date-day) 1)))

(define-public (start-of-day* time)
  (date->time-utc (drop-time (time-utc->date time))))

(define (make-duration s)
  (make-time time-duration 0 s))

(define (in-day? day-date time)
  (let* ((now (date->time-utc (drop-time day-date)))
         (then (add-duration now (make-duration (* 60 60 24)))))
    (and (time<=? now time)
         (time<=? time then))))

(define (today? time)
  (in-day? (current-date) time))

(define* (time->string time #:optional (format "~1 ~3"))
  (date->string (time-utc->date time) format))

;; TODO these ({add,remove}-day} might have problem moving between timezones.

(define (add-day time)
  (add-duration time (make-duration (* 60 60 24))))

(define (remove-day time)
  (add-duration time (make-duration (- (* 60 60 24)))))


(define-public (normalize-date date)
  (time-utc->date (date->time-utc date)
                  (zone-offset date)))

;; Normalize a date on a weird form back into a propper form,
;; for example, 2019-11-32 becomes 2019-12-02.
;; The whole adding 10 seconds and then removing them is a dirty
;; hack to handle leap seconds. NOTE this should be reworked.
(define-public (normalize-date* date)
  (define next-date
    (time-utc->date
     (add-duration (date->time-utc date)
                   (make-time time-duration 0 10))))
  (set (date-second next-date) 0))

(define-public (normalize-date** date)
  (define next-date
    (time-utc->date
     (subtract-duration (date->time-utc date)
                        (make-time time-duration 0 7200))))
  (set (date-second next-date) 0))

(define*-public (normalize-date/tz date #:optional (tz "Europe/Stockholm"))
  (let-env ((TZ tz))
           (-> date date->time-utc time-utc->date)))

;; date x (date → date) → stream<date>
(define (date-increment-stream* start-date transfer-proc)
  (stream-iterate
   (lambda (d)
     (drop-time
      (normalize-date*
       ;; NOTE Adds one hour to compensate for summer -> winter time transition
       ;; TODO figure out better way to do this.
       (set (date-hour (transfer-proc d)) = (+ 1)))))
   (drop-time start-date)))

;; Just dropping timezones seems to work when we are dealing with months...
(define (date-increment-stream start-date transfer-proc)
  (stream-iterate
   (lambda (d)
     (drop-time
      (normalize-date
       (transfer-proc d))))
   (drop-time start-date)))

;; Returns a stream of date objects, one day appart, staring from start-day.
(define-public (day-stream start-day)
  (date-increment-stream* start-day (lambda (d) (set (date-day d) = (+ 1)))))

(define-public (month-stream start-date)
  (date-increment-stream start-date (lambda (d) (set (date-month d) = (+ 1)))))

(define-public (in-date-range? start-date end-date)
  (lambda (date)
    (let ((time (date->time-utc date)))
      (and (time<=? (date->time-utc start-date) time)
           (time<=? time (date->time-utc end-date))))))

(define-public (time-min a b)
  (if (time<? a b) a b))

(define-public (time-max a b)
  (if (time<? a b) b a))


;; TODO possibly put this in some form of parser module.
;; TODO actually allow many form date form.
(define-public (parse-freeform-date str)
  (string->date str "~Y-~m-~d"))


;; Easier constructor for date objects
;; Default values set to the begining of time.
(define* (date #:key
               (year 1970) (month 1) (day 1)
               (hour 0) (minute 0) (second 0)
               (nsecs 0) (tz 0))
  (make-date nsecs second minute hour day month year tz))


(define-public (date=? a b)

  (and (= (year a) (year b))
       (= (month a) (month b))
       (= (day a) (day b))
       ;; (= (hour a) (hour b))
       ;; (= (minute a) (minute b))
       ;; (= (second a) (second b))
       )
  ;; ( (nsecs b) (zone b))
  )

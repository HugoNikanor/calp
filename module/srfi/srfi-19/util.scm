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

;; @verbatim
;;    A          B          C          D          E         ¬F
;; |s1|     :     |s2| : |s1|     :     |s2| :          : |s1|
;; |  |     :     |  | : |  ||s2| : |s1||  | : |s1||s2| : |  |
;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | : |  ||  | :
;;     |  | : |  |     : |  ||  | : |  ||  | : |  ||  | :     |s2|
;;     |  | : |  |     : |  |     :     |  | :          :     |  |
;; @end verbatim
;; 
;; E is covered by both case A and B.
(define-public (timespan-overlaps? s1-begin s1-end s2-begin s2-end)
  "Return whetever or not two timespans overlap."
  (or
   ;; A
   (and (time<? s2-begin s1-end)
        (time<? s1-begin s2-end))

   ;; B
   (and (time<? s1-begin s2-end)
        (time<? s2-begin s1-end))

   ;; C
   (and (time<? s1-begin s2-begin)
        (time<? s2-end s1-end))

   ;; D
   (and (time<? s2-begin s1-begin)
        (time<? s1-end s2-end))))

(define-public (normalize-date date)

  (time-utc->date (date->time-utc date)
                  (zone-offset date)))

;; Normalize a date on a weird form back into a propper form,
;; for example, 2019-11-32 becomes 2019-12-02.
;; The whole adding 10 seconds and then removing them is a dirty
;; hack to handle leap seconds. NOTE this should be reworked.
(define-public (normalize-date* date)
  (define next-date
    (date-second
     (time-utc->date
      (add-duration (date->time-utc date)
                    (make-time time-duration 0 10)))
     (set next-date 0))))

;; Returns a stream of date objects, one day appart, staring from start-day.
(define-public (day-stream start-day)
  (stream-iterate
   (lambda (d)
     (drop-time
      (normalize-date*
       (set (date-day d) = (+ 1)))))
   (drop-time start-day)))

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

(define* (date #:key (year 0) (month 0) (day 0) (hour 0) (minute 0) (second 0) (nsecs 0) (zone 0))
  (make-date nsecs second minute hour day month year zone))

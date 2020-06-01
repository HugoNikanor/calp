(define-module (datetime util)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (ice-9 i18n)
  :use-module (ice-9 format)
  :use-module (util)
  :use-module (util config)
  :re-export (locale-month)
  )

(define-public (start-of-month date)
  (set (day date) 1))

(define-public (end-of-month date)
  (set (day date) (days-in-month date)))

(define-public (start-of-year date)
  (set-> date
         (day 1)
         (month 1)))

(define-public (parse-freeform-date str)
  (let* (((year month day) (map string->number (string-split str #\-))))
    (date year: year month: month day: day)
    ))

(define-public (date-stream date-increment start-day)
  (stream-iterate (cut date+ <> date-increment)
                  start-day))

(define-public (day-stream start-day)
  (date-stream (date day: 1) start-day))

(define-public (month-stream start-day)
  (date-stream (date month: 1) start-day))

(define-public (week-stream start-day)
  (date-stream (date day: 7) start-day))

(define-public (time-min a b)
  (if (time<? a b) a b))

(define-public (time-max a b)
  (if (time<? a b) b a))

(define-public (date-min a b)
  (if (date< a b) a b))

(define-public (date-max a b)
  (if (date< a b) b a))

(define-public (datetime-min a b)
  (if (datetime< a b) a b))

(define-public (datetime-max a b)
  (if (datetime< a b) b a))

(define*-public (month+ date-object #:optional (change 1))
  (date+ date-object (date month: change)))

(define*-public (month- date-object #:optional (change 1))
  (date- date-object (date month: change)))

;; https://projecteuclid.org/euclid.acta/1485888738
;; 1. Begel.
;; J sei die Zahl des Jahrhunderts,
;; K die Jahrszahl innerhalb desselben,
;; m die Zahl des Monats,
;; q die Zahl des Monatstags,
;; h die Zahl des Wochentags;
(define (zeller J K m q)
  (modulo (+ q
             (floor-quotient (* 13 (1+ m))
                             5)
             K
             (floor-quotient K 4)
             5
             (- J))
          7))

;; 0 indexed, starting at sunday.
(define-public (week-day date)
  (let* ((J K (floor/ (year date) 100))
         (m (month date)))
    (if (memv m '(1 2))
        (zeller J (1- K) (+ m 12) (day date))
        (zeller J K (month date) (day date)))))


(define-many define-public
  (sun) 0
  (mon) 1
  (tue) 2
  (wed) 3
  (thu) 4
  (fri) 5
  (sat) 6
  )


(define-public week-start (make-parameter sun))

(define-config week-start sun
  "First day of week"
  pre: (ensure (lambda (x) (<= sun x sat)))
  post: week-start)

;; given a date, returns the date the first week of that year starts on.
;; @example
;; (week-1-start #2020-01-01 mon)
;; ⇒ 2019-12-30
;; @end example
(define*-public (week-1-start d optional: (wkst (week-start)))
  (let* ((ystart (start-of-year d))
         (day-index (modulo (- (week-day ystart) wkst) 7)))
    (if (> day-index 3)
        (date+ ystart (date day: (- 7 day-index)))
        (date- ystart (date day: day-index)))))

;; (week-number #2020-01-01 mon) ; => 1
;; (week-number #2019-12-31 mon) ; => 1
(define*-public (week-number d optional: (wkst (week-start)))
  ;; Calculating week number for starts of week was much simpler.
  ;; We can both skip the special cases for Jan 1, 2 & 3. It also
  ;; solved some weird bug that was here before.

  (let ((d (start-of-week d wkst)))
   (cond
    [(and (= 12 (month d))
          (memv (day d) '(29 30 31))
          (< (year d) (year (date+ (start-of-week d wkst)
                                   (date day: 3)))))
     1]

    [else
     (let* ((w1-start (week-1-start d wkst))
            (week day (floor/ (days-in-interval w1-start d)
                              7)))
       (1+ week))])))

(define*-public (date-starting-week week-number d optional: (wkst (week-start)))
  (date+ (week-1-start d wkst)
         (date day: (* (1- week-number) 7))))


(define*-public (week-day-name week-day-number optional: truncate-to
                               key: (locale %global-locale))

  ;; NOTE this allows days larger than 7 (sunday if counting from monday).
  (let ((str (catch 'out-of-range
               (lambda () (locale-day (1+ (modulo week-day-number 7)) locale))
               (lambda (oor str num) (scm-error 'out-of-range 'week-day-name
                                           "~a == (~a % 7) + 1"
                                           (list num week-day-number) (list week-day-number))))))
    ;; I also know about the @var{locale-day-short} method, but I need
    ;; strings of length 2.
    (if truncate-to
        (string-take str truncate-to)
        str)))

(define*-public (datetime->string datetime optional: (fmt "~Y-~m-~dT~H:~M:~S") key: allow-unknown?)
  (define date (get-date datetime))
  (define time ((@ (datetime) get-time%) datetime))
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\H) (format #t "~2'0d" (hour time)))
                   ((#\k) (format #t "~2' d" (hour time)))
                   ((#\M) (format #t "~2'0d" (minute time)))
                   ((#\S) (format #t "~2'0d" (second time)))
                   ((#\Y) (format #t "~4'0d" (year date)))
                   ((#\m) (format #t "~2'0d" (month date)))
                   ((#\d) (format #t "~2'0d" (day date)))
                   ;; Should be same as ~_d
                   ((#\s) (display (datetime->unix-time datetime))) ; epoch time!
                   ((#\e) (format #t "~2' d" (day date)))
                   ((#\1) (format #t "~4'0d-~2'0d-~2'0d"
                                  (year date) (month date) (day date)))
                   ((#\3) (format #t "~2'0d:~2'0d:~2'0d"
                                  (hour time) (minute time) (second time)))
                   ((#\A) (display (week-day-name (week-day date))))
                   ((#\a) (display (week-day-name (week-day date) 3)))
                   ((#\b) (display (locale-month-short (month date))))
                   ((#\B) (display (locale-month (month date))))
                   ((#\Z) (when (equal? "UTC" (get-timezone datetime)) (display "Z")))
                   (else (unless allow-unknown?
                           (error 'datetime->string "Invalid format token ~a" token))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

(define*-public (date->string date optional: (fmt "~Y-~m-~d") key: allow-unknown?)
  (datetime->string (datetime date: date) fmt allow-unknown?: allow-unknown?))

(define*-public (time->string time optional: (fmt "~H:~M:~S") key: allow-unknown?)
  (datetime->string (datetime time: time) fmt allow-unknown?: allow-unknown?))


;; @verbatim
;;    A          B          C          D          E         ¬F
;; |s1|     :     |s2| : |s1|     :     |s2| :          : |s1|
;; |  |     :     |  | : |  ||s2| : |s1||  | : |s1||s2| : |  |
;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | : |  ||  | :
;;     |  | : |  |     : |  ||  | : |  ||  | : |  ||  | :     |s2|
;;     |  | : |  |     : |  |     :     |  | :          :     |  |
;;
;; Infinitely short ---+|s2| : |s1|+--- : two instants don't overlap
;; events, overlap   s1      :      s2  :
;; @end verbatim
;; 
;; E is covered by both case A and B.
(define-public (timespan-overlaps? s1-begin s1-end s2-begin s2-end)
  "Return whetever or not two timespans overlap."
  (or
   ;; A
   (and (date/-time<? s2-begin s1-end)
        (date/-time<? s1-begin s2-end))

   ;; B
   (and (date/-time<? s1-begin s2-end)
        (date/-time<? s2-begin s1-end))

   ;; C
   (and (date/-time<=? s1-begin s2-begin)
        (date/-time<? s2-end s1-end))

   ;; D
   (and (date/-time<=? s2-begin s1-begin)
        (date/-time<? s1-end s2-end))))

(define-public (add-day d)
  (date+ d (date day: 1)))

(define-public (remove-day d)
  (date- d (date day: 1)))

(define-public (in-date-range? start-date end-date)
  (lambda (date)
    (date<= start-date date end-date)))

;; Returns a list of the seven week days, with @var{week-start}
;; as the beginning of the week.
;; @example
;; '(SÖ MÅ TI ON TO FR LÖ)
;; @end example
(define-public (weekday-list week-start)
  (take (drop (apply circular-list (iota 7))
              week-start)
        7))

;; returns the date the week containing d started.
;; (start-of-week #2020-04-02 sun) ; => 2020-03-29
(define*-public (start-of-week d optional: (week-start (week-start)))
  (date- d (date day: (modulo (- (week-day d)
                                 week-start)
                              7))))

;; (end-of-week #2020-04-01 mon)
;; => 2020-04-05
(define*-public (end-of-week d optional: (week-start (week-start)))
  (date+ (start-of-week d week-start)
         (date day: 6)))


;; Given a month and and which day the week starts on,
;; returns three lists, which are:
;; The days leading up to the current month, but share a week
;; The days in the current month
;; The days after the current month, but which shares a week.
;; 
;;       mars 2020
;; må ti on to fr lö sö
;;                    1
;;  2  3  4  5  6  7  8
;;  9 10 11 12 13 14 15
;; 16 17 18 19 20 21 22
;; 23 24 25 26 27 28 29
;; 30 31
;; @lisp
;; (month-days #2020-03-01 mon)
;; ; ⇒ (2020-02-24 ... 2020-02-29)
;; ; ⇒ (2020-03-01 ... 2020-03-31)
;; ; ⇒ (2020-04-01 ... 2020-04-05)
;; @end lisp
;; Ignores day component of @var{date}.
(define*-public (month-days date optional: (week-start (week-start)))
  (let* ((month-len (days-in-month date))
         (prev-month-len (days-in-month (month- date)))
         (month-start (modulo (- (week-day date) week-start) 7)))
    (values
     (map (lambda (d) (set (day (month- date)) d))
          (iota month-start (1+ (- prev-month-len month-start))))
     (map (lambda (d) (set (day date) d)) (iota month-len 1))
     (map (lambda (d) (set (day (month+ date)) d))
          (iota (modulo (- (* 7 5) month-len month-start) 7) 1)))))




(define-public (days-in-interval start-date end-date)
  (let ((diff (date-difference (date+ end-date (date day: 1)) start-date)))
    (with-streams
     (fold + (day diff)
           (map days-in-month
                (take (+ (month diff)
                         (* 12 (year diff)))
                      (month-stream start-date)))))))

;; Day from start of the year, so 1 feb would be day 32.
;; Also known as Julian day.
(define-public (year-day date)
  (days-in-interval (start-of-year date) date))


;; @example
;; (time->decimal-hour #10:30:00) ; => 10.5
;; @end example
(define-public (time->decimal-hour time)
  (exact->inexact (+ (hour time)
                     (/ (minute time) 60)
                     (/ (second time) 3600))))

(define*-public (datetime->decimal-hour dt optional: start-date)

  (let ((date-diff
         (cond [start-date
                (let* ((end-date (date+ start-date (get-date dt))))
                  (days-in-interval start-date end-date)) ]
               [(or (not (zero? (month (get-date dt))))
                    (not (zero? (year (get-date dt)))))
                (error "Multi-month intervals only supported when start-date is given" dt)]
               [else (day (get-date dt))])))
    (+ (time->decimal-hour ((@ (datetime) get-time%) dt))
       (* (1- date-diff) 24))))

;; Returns a list of all dates from start to end.
;; both inclusive
;; date, date → [list date]
(define-public (date-range start end)
  (stream->list
   (stream-take-while (lambda (d) (date<= d end))
                      (day-stream start))))



;; TODO normalize if functions floor their arguments or not.
;; The argument for flooring is that it allows us to only bother with
;; the higher components we care about.
;; The argument against would be if we want to start from the middle
;; of a time span.


;; Returns the first instance of the given week-day in the given month.
;; @example
;; (find-first-week-day mon #2020-04-10)
;; => 2020-04-06
;; @end example
(define-public (find-first-week-day wday month-date)
  (let* ((mstart (start-of-month month-date))
         (start-day (week-day mstart))
         (diff (- wday start-day)))
    (date+ mstart (date day: (modulo diff 7)))))

;; returns instances of the given week-day in month.
;; week-day, date → (list date)
(define-public (all-wday-in-month wday month-date)
  (stream->list
   (stream-take-while
    (lambda (d) (= (month d) (month month-date)))
    (week-stream (find-first-week-day wday month-date)))))


(define-public (all-wday-in-year wday year-date)
  (stream->list
   (stream-take-while
    (lambda (d) (= (year d) (year year-date)))
    (week-stream (find-first-week-day wday year-date)))))

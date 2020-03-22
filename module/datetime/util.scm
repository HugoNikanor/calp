(define-module (datetime util)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (util)
  )

(define-public (start-of-month date)
  (set (day date) 1))


(define-public (parse-freeform-date str)
  (let* (((year month day) (map string->number (string-split str #\-))))
    (date year: year month: month day: day)
    ))

(define-public (day-stream start-day)
  (stream-iterate (cut date+ <> #0-0-1)
                  start-day))

(define-public (month-stream start-day)
  (stream-iterate (cut date+ <> #0-1-0)
                  start-day))

(define-public (week-stream start-day)
  (stream-iterate (cut date+ <> (date day: 7))
                  start-day))

(define-public (time-min a b)
  (if (time<? a b) a b))

(define-public (time-max a b)
  (if (time<? a b) b a))

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

;; Given a date, returns the earliest start of week going backwards from that date.
;; sön 22 mar 2020 20:09:57 CET
;; @example
;; (previous-week-start #2020-03-22 mon)
;; => 2020-03-16
(define-public (previous-week-start date* week-start)
  ((@ (srfi srfi-41 util) stream-find)
   (lambda (d) (= week-start (week-day d)))
   ((@ (srfi srfi-41) stream-iterate) (cut date- <> (date day: 1))
    date*)))

(define-many define-public
  (sun) 0
  (mon) 1
  (tue) 2
  (wed) 3
  (thu) 4
  (fri) 5
  (sat) 6
  )

(define*-public (week-day-name week-day-number optional: truncate-to)
  ;; TODO internationalization
  (let ((str
         (case* week-day-number
                [(sun 7) "Söndag"]
                [(mon) "Måndag"]
                [(tue) "Tisdag"]
                [(wed) "Onsdag"]
                [(thu) "Torsdag"]
                [(fri) "Fredag"]
                [(sat) "Lördag"]
                [else (error 'argument-error "No day ~a in week" week-day-number)])))
    (if truncate-to
        (string-take str truncate-to)
        str)))

(define (month-name month)
  (case month
    [(1) "Jan"]
    [(2) "Feb"]
    [(3) "Mar"]
    [(4) "Apr"]
    [(5) "Maj"]
    [(6) "Jun"]
    [(7) "Jul"]
    [(8) "Aug"]
    [(9) "Sep"]
    [(10) "Okt"]
    [(11) "Nov"]
    [(12) "Dec"]
    [else (error "No month ~a" month)]))

(define*-public (date->string date optional: (fmt "~Y-~m-~d") key: allow-unknown?)
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ;; TODO add #\_ to change pad to spaces
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\Y) (format #t "~4'0d" (year date)))
                   ((#\m) (format #t "~2'0d" (month date)))
                   ((#\d) (format #t "~2'0d" (day date)))
                   ;; Should be same as ~_d
                   ((#\e) (format #t "~2' d" (day date)))
                   ((#\1) (format #t "~4'0d-~2'0d-~2'0d"
                                  (year date) (month date) (day date)))
                   ((#\a) (display (week-day-name (week-day date))))
                   ;; abriviated locale month name
                   ;; TODO locale
                   ((#\b) (display (month-name (month date))))
                   (else (unless allow-unknown?
                           (error 'date->string "Invalid format token ~a" token))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

;; (define*-public (time->string time optional: (fmt "~H:~M:~S") key: allow-unknown?)
;;   (with-output-to-string
;;     (lambda ()
;;       (fold (lambda (token state)
;;               (case state
;;                 ((#\~)
;;                  (case token
;;                    ((#\~) (display "~"))
;;                    ((#\H) (format #t "~2'0d" (hour time)))
;;                    ((#\M) (format #t "~2'0d" (minute time)))
;;                    ((#\S) (format #t "~2'0d" (second time)))
;;                    ;; ((#\z) (when (utc? time) (display "Z")))
;;                    (else (unless allow-unknown?
;;                            (error 'time->string "Invalid format token ~a" token))))
;;                  #f)
;;                 (else (unless (char=? #\~ token) (display token)) token)))
;;             #f
;;             (string->list fmt)))))

(define*-public (datetime->string datetime optional: (fmt "~Y-~m-~dT~H:~M:~S") key: allow-unknown?)
  (define dt (get-datetime datetime))
  (define date (get-date dt))
  (define time ((@ (datetime) get-time%) dt))
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\H) (format #t "~2'0d" (hour time)))
                   ((#\M) (format #t "~2'0d" (minute time)))
                   ((#\S) (format #t "~2'0d" (second time)))
                   ;; TODO
                   ;; ((#\z) (when (utc? time) (display "Z")))
                   ((#\Y) (format #t "~4'0d" (year date)))
                   ((#\m) (format #t "~2'0d" (month date)))
                   ((#\d) (format #t "~2'0d" (day date)))
                   ;; Should be same as ~_d
                   ((#\e) (format #t "~2' d" (day date)))
                   ((#\1) (format #t "~4'0d-~2'0d-~2'0d"
                                  (year date) (month date) (day date)))
                   ((#\a) (display (week-day-name (week-day date))))
                   ;; abriviated locale month name
                   ;; TODO locale
                   ((#\b) (display (month-name (month date))))
                   (else (unless allow-unknown?
                           (error 'datetime->string "Invalid format token ~a" token))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))



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
   (and (date/-time<? s2-begin s1-end)
        (date/-time<? s1-begin s2-end))

   ;; B
   (and (date/-time<? s1-begin s2-end)
        (date/-time<? s2-begin s1-end))

   ;; C
   (and (date/-time<? s1-begin s2-begin)
        (date/-time<? s2-end s1-end))

   ;; D
   (and (date/-time<? s2-begin s1-begin)
        (date/-time<? s1-end s2-end))))

(define-public (add-day d)
  (date+ d (date day: 1)))

(define-public (remove-day d)
  (date- d (date day: 1)))


;; Checks if @var{datetime} is within the date
;; given by @var{base-date}.
;; TODO test time zones
;; date x datetime → bool
;; (define-public (in-day? base-date date/-time)
;;   (date< base-date (as-date date/-time) (date+ base-date (date day: 1))))

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
;; (month-days #2020-03-01 mon)
;; => (2020-02-24 ... 2020-02-29)
;; => (2020-03-01 ... 2020-03-31)
;; => (2020-04-01 ... 2020-04-05)
;; TODO Currently givining a non-start-of-month date for @var{date} is an error.
(define-public (month-days date week-start)
  (let* ((month-len (days-in-month date))
         (prev-month-len (days-in-month (month- date)))
         (month-start (modulo (- (week-day date) week-start) 7)))
    (values
     (map (lambda (d) (set (day (month- date)) d))
          (iota month-start (1+ (- prev-month-len month-start))))
     (map (lambda (d) (set (day date) d)) (iota month-len 1))
     (map (lambda (d) (set (day (month+ date)) d))
          (iota (modulo (- (* 7 5) month-len month-start) 7) 1)))))


(define-module (srfi srfi-19 alt util)
  :use-module (srfi srfi-19 alt)
  :use-module ((srfi srfi-1) :select (fold))
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

(define-public (time-min a b)
  (if (time<? a b) a b))

(define-public (time-max a b)
  (if (time<? a b) b a))


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

(define-public (week-day-name week-day-number)
  ;; TODO internationalization
  (case* week-day-number
         [(sun 7) "Sön"]
         [(mon) "Mån"]
         [(tue) "Tis"]
         [(wed) "Ons"]
         [(thu) "Tor"]
         [(fri) "Fre"]
         [(sat) "Lör"]))

(define*-public (date->string date optional: (fmt "~Y-~m-~d") key: allow-unknown?)
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\Y) (format #t "~4'0d" (year date)))
                   ((#\m) (format #t "~2'0d" (month date)))
                   ((#\d) (format #t "~2'0d" (day date)))
                   ((#\1) (format #t "~4'0d-~2'0d-~2'0d"
                                  (year date) (month date) (day date)))
                   ((#\a) (display (week-day-name (week-day date))))
                   (else (unless allow-unknown?
                           (error 'date->string "Invalid format token ~a" token))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

(define*-public (time->string time optional: (fmt "~H:~M:~S") key: allow-unknown?)
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
                   (else (unless allow-unknown?
                           (error 'time->string "Invalid format token ~a" token))))
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
    ;; (format (current-error-port) "in-date-range? ~a < ~a < ~a = ~a~%"
    ;;         (date->string start-date)
    ;;         (date->string date)
    ;;         (date->string end-date)
    ;;         (date<= start-date date end-date) )
    (date<= start-date date end-date)))

(define-module (srfi srfi-19 alt util)
  :use-module (srfi srfi-19 alt)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (util)
  )

(define-public (start-of-month date)
  (set (day date) 0))


(define-public (parse-freeform-date str)
  (let* (((year month day) (map string->number (string-split str #\-))))
    (date year: year month: month day: day)
    ))

(define-public (day-stream start-day)
  (stream-iterate (cut date+ <> #0-0-1)
                  start-day))

(define (as-date date/-time)
  (if (date? date/-time)
      date/-time
      (get-date date/-time)))

(define (as-time date/-time)
  (if (datetime? date/-time)
      (get-time date/-time)
      #00:00:00))

(define-public (date/-time< a b)
  (if (date< (as-date a) (as-date b))
      #t
      (time< (as-time a) (as-time b))))

(define-public date/-time<? date/-time<)

(define*-public (date->string date optional: (fmt "~Y-~m-~d"))
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
                   (else (error "Invalid format token ~a" token)))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

(define*-public (time->string time optional: (fmt "~H:~M:~S"))
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\H) (format #t "~2'0d" (hour date)))
                   ((#\M) (format #t "~2'0d" (minute date)))
                   ((#\S) (format #t "~2'0d" (second date)))
                   (else (error "Invalid format token ~a" token)))
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

(define-public (add-day date)
  (date+ date (date day: 1)))

(define-public (remove-day date)
  (date- date (date day: 1)))

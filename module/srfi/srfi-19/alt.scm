(define-module (srfi srfi-19 alt)
  :export (date? year month day
                 hour minute second
                 time? datetime?
                 )

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (ice-9 match)

  :use-module (util)
  )

(define-many define-public
  (jan january   ) 1
  (feb february  ) 2
  (mar mars      ) 3
  (apr april     ) 4
  (may           ) 5
  (jun june      ) 6
  (jul july      ) 7
  (aug august    ) 8
  (sep september ) 9
  (oct october   ) 10
  (nov november  ) 11
  (dec december  ) 12
  )

(define-immutable-record-type <date>
  (make-date year month day)
  date?
  (year year) (month month) (day day))

(set-record-type-printer!
 <date>
 (lambda (r p)
   (if (or (not (integer? (year r)))
           (not (integer? (month r)))
           (not (integer? (day r))))
       (format p "BAD~s-~s-~s" (year r) (month r) (day r))
       (format p "~4'0d-~2'0d-~2'0d"
               (year r) (month r) (day r)))))

(define*-public (date key: (year 0) (month 0) (day 0))
  (make-date year month day))


;; int -> bool
(define-public (leap-year? year)
  (and (zero? (remainder year 4))
       (or (zero? (remainder year 400))
           (not (zero? (remainder year 100))))))

;; Returns number of days month for a given date. Just looks at the year and month components.
(define-public (days-in-month date)
  (case* (month date)
         ((jan mar may jul aug oct dec) 31)
         ((apr jun sep nov) 30)
         ((feb)
          (if (leap-year? (year date))
              29 28))))

(define-public (days-in-year date)
  (if (leap-year? (year date))
      366 365))

;; 2020-01-10 + 0-0-30 = 2020-02-09
;; 10 + 30 = 40                            ; day + day
;; 40 > 31                                 ; target days > days in month
;; 2020-02-00 + 0-0- (40 - 31)             ;
;; 2020-02-09

(define-public (date= a b)
  (and (= (year a) (year b))
       (= (month a) (month b))
       (= (day a) (day b))))

(define-public date=? date=)

(define (date+% base change)

  ;; while (day base) > (days-in-month base)
  ;;     month++; days -= (days-in-month base)
  (define days-fixed
    (let loop ((target (set (day base) = (+ (day change)))))
      (if (> (day target) (days-in-month target))
          (loop (set-> target
                       (month = (+ 1))
                       (day = (- (days-in-month target)))))
          target)))

  ;; while (month base) > 12
  ;;     year++; month -= 12
  (define months-fixed
    (let loop ((target (set (month days-fixed) = (+ (month change)))))
      (if (> (month target) 12)
          (loop (set-> target
                       (year = (+ 1))
                       (month = (- 12))))
          target)))

  (set (year months-fixed) = (+ (year change))))

(define-public (date+ base . rest)
  (fold date+% base rest))

(define-public (date- base change)

  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (day change) (day target))
          (loop (set-> target
                       (month = (- 1))
                       (day (days-in-month (set (month target) = (- 1)))))
                (set (day change) = (- (day target))))
          (values (set (day target) = (- (day change)))
                  (set (day change) 0)))))

  (define-values (month-fixed change**)
    (let loop ((target days-fixed) (change change*))
      (if (>= (month change) (month target))
          (loop (set-> target
                       (year = (- 1))
                       (month 12))
                (set (month change) = (- (month target))))
          (values (set (month target) = (- (month change)))
                  (set (month change) 0)))))

  ;; change** should here should have both month and date = 0

  (set (year month-fixed) = (- (year change))))

(define-public (time- base change)

  (define-values (second-fixed change*)
    (let loop ((target base) (change change))
      (if (> (second change) (second target))
          (loop (set-> target
                       (minute = (- 1))
                       (second 60))
                (set (second change) = (- (second target))))
          (values (set (second target) = (- (second change)))
                  (set (second change) 0)))))

  (define-values (minute-fixed change**)
    (let loop ((target second-fixed) (change change*))
      (if (> (minute change) (minute target))
          (loop (set-> target
                       (hour = (- 1))
                       (minute 60))
                (set (minute change) = (- (minute target))))
          (values (set (minute target) = (- (minute change)))
                  (set (minute change) 0)))))

  ;; change** should here should have both month and date = 0

  (set (hour month-fixed) = (- (hour change)))
  )



(define-immutable-record-type <time>
  (make-time hour minute second utc)
  time?
  (hour hour) (minute minute) (second second)
  (utc utc)                             ; bool
  )

(set-record-type-printer!
 <time>
 (lambda (r p)
   (format p "~2'0d:~2'0d:~2'0d~a"
           (hour r) (minute r) (second r)
           (if (utc r) "Z" ""))))

(define-public (time->string time _)
  (with-output-to-string (lambda () (display time))))

(define*-public (time key: (hour 0) (minute 0) (second 0) (utc #f))
  (make-time hour minute second utc))

(define-public (time+% base change)

  ;; while (day base) > (days-in-month base)
  ;;     month++; days -= (days-in-month base)
  (define second-fixed
    (let loop ((target (set (second base) = (+ (second change)))))
      (if (>= (second target) 60)
          (loop (set-> target
                       (minute = (+ 1))
                       (second = (- 60))))
          target)))

  ;; while (month base) > 12
  ;;     year++; month -= 12
  (define minute-fixed
    (let loop ((target (set (minute second-fixed) = (+ (minute change)))))
      (if (>= (minute target) 60)
          (loop (set-> target
                       (hour = (+ 1))
                       (minute = (- 60))))
          target)))

  ;;  ahtns auoe htns a oeuhnstaoue nhts aoeu nshtaoeu snht oeuia htns oaeu nsht aoeuö ntshaouentsh oaesnuthg aoeu nsthaoeu nshtaou eshtnnh toaeuhnst oeuhtns 
  (define hour-almost-fixed (set (hour minute-fixed) = (+ (hour change))))

  ;; (format #t "~s ~s ~s~%" second-fixed minute-fixed hour-almost-fixed)

  (if (<= 24 (hour hour-almost-fixed))
      (let* ((div remainder (floor/ (hour hour-almost-fixed) 24)))
        (values (set (hour hour-almost-fixed) remainder) div))
      (values hour-almost-fixed 0)))

(define-public (time+ base . rest)
  (fold time+% base rest))

(define-immutable-record-type <datetime>
  (make-datetime date time tz)
  datetime?
  (date get-date set-date!)
  (time get-time set-time!)
  (tz get-tz set-tz!))

(export get-date get-time get-tz)

(define*-public (make-datetime*
                 key:
                 (year 0) (month 0) (day 0)
                 (hour 0) (minute 0) (second 0)
                 (tz #f))
  (make-datetime (make-date year month day)
                 (make-time hour minute second #f)
                 tz))

(define-public datetime
  (match-lambda*
    [(date time) (make-datetime date time #f)]
    [(date time tz) (make-datetime date time #f)]
    [args (apply make-datetime* args)]))

(define-public (datetime+ base change)
  (let* ((time overflow (time+ (get-time base) (get-time change))))
    (date+ (get-date base)
           (get-date change)
           (date day: overflow))))



(define-public (date< a b)
  (let ((ay (year a))
        (by (year b)))
    (if (= ay ay)
        (let ((am (month a))
              (bm (month b)))
          (if (= am bm)
              (< (day a) (day b))
              (< am bm)))
        (< ay by))))


(define-public (time< a b)
  (let ((ah (hour a))
        (bh (hour b)))
    (if (= ah ah)
        (let ((am (minute a))
              (bm (minute b)))
          (if (= am bm)
              (< (second a) (second b))
              (< am bm)))
        (< ah bh))))


(define-public (datetime< a b)
  (if (date= (get-date a) (get-date b))
      (time< (get-time a) (get-time b))
      (date< (get-date a) (get-date b))))


(define-many define-public
  (date<?) date<
  (date> date>?) (swap date<)

  (time<?) time<
  (time> time>?) (swap time<)

  (datetime<?) datetime<
  (datetime> datetime>?) (swap datetime<))



;; substring to number, local here
(define (s->n str from to)
  (string->number (substring/read-only str from to)))

(define-public (parse-date str)
  (date year:  (s->n str 0 4)
        month: (s->n str 4 6)
        day:   (s->n str 6 8)))

(define-public (parse-time str)
  (time hour:   (s->n str 0 2)
        minute: (s->n str 2 4)
        second: (s->n str 4 6)
        ;; TODO UTC
        ))

(define-public (parse-datetime str)
  (let* (((datestr timestr) (string-split str #\T)))
    (datetime (parse-date datestr)
              (parse-time timestr))))


(define-public (current-date)
  (let ((d ((@ (srfi srfi-19) current-date))))
    (date year:  ((@ (srfi srfi-19) date-year) d)
          month: ((@ (srfi srfi-19) date-month) d)
          day:   ((@ (srfi srfi-19) date-day) d))))





;; Reader extensions

(define (parse-date% str)
  (let* (((year month day) (map string->number (string-split str #\-))))
    `(date year: ,year month: ,month day: ,day)))

(define (parse-time% timestr)
  (let* (((hour minute second) (string-split timestr #\:)))
    (let ((utc? (string-contains second "Z")))
      (let ((hour (string->number hour))
            (minute (string->number minute))
            (second (string->number (if utc? (string-drop-right second 1) second))))
        `(time hour: ,hour minute: ,minute second: ,second utc: ,utc?)))))

(define (parse-datetime% str)
  (let* (((date time) (string-split str #\T)))
    `(datetime ,(parse-date date)
               ,(parse-time time))))

(define (date-reader chr port)
  (unread-char chr port)
  (let ((line (symbol->string (read port))))
    (cond [(string-contains line "T") (parse-datetime% line)]
          [(string-contains line ":") (parse-time% line)]
          [(string-contains line "-") (parse-date% line)])))

(read-hash-extend #\0 date-reader)
(read-hash-extend #\1 date-reader)
(read-hash-extend #\2 date-reader)

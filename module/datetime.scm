(define-module (datetime)
  :export (date? year month day
                 hour minute second
                 time? datetime?)
  ;; To resolve colision with cadr-second from srfi-1
  :replace (second)

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


;;; RECORD TYPES

;;; DATE

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

;;; TIME

(define-immutable-record-type <time>
  (make-time hour minute second)
  time?
  (hour hour) (minute minute) (second second))

(set-record-type-printer!
 <time>
 (lambda (r p)
   (if (or (not (integer? (hour r)))
           (not (integer? (minute r)))
           (not (integer? (second r))))
       (format p "BAD~s:~s:~s"
               (hour r) (minute r) (second r))
       (format p "~2'0d:~2'0d:~2'0d"
               (hour r) (minute r) (second r)))))

(define*-public (time key: (hour 0) (minute 0) (second 0))
  (make-time hour minute second))

;;; DATETIME

(define-immutable-record-type <datetime>
  (make-datetime date time tz)
  datetime?
  (date get-date)
  (time get-time%)
  (tz tz) ; #f, 'UTC, 'Z
  )

(export get-date)

(define*-public (datetime
                 key: date time
                 (year 0) (month 0) (day 0)
                 (hour 0) (minute 0) (second 0)
                 tz)
  (make-datetime (or date (make-date year month day))
                 (or time (make-time hour minute second))
                 tz))

;; datetime → datetime
;; Takes a datetime in any timezone, and renormalize it to local time
;; (as defined by TZ). This means that given UTC 10:00 new years day
;; would return 11:00 new years day if ran in sweden.
(define-public (get-datetime dt)
  (let ((t (get-time% dt))
        (d (get-date dt)))
    ;; NOTE there isn't any stable way to craft the tm objects.
    ;; I could call mktime on some date, and replace the fields
    ;; with the set-tm:*, but that is worse that breaking the API.
    (let ((v (vector (second t)
                     (minute t)
                     (hour t)
                     (day d)
                     (1- (month d))
                     (- (year d) 1900)
                     0 0                ; wday & yday (ignored)
                     -1                 ; DST unknown
                     0                  ; UTC offset (ignored)
                     #f                 ; TZ name
                     )))
      (let ((tm
             (localtime ; localtime convertion since the returned tm object is
              (car      ; in the parsed timezone.
               (cond [(not (tz dt)) (mktime v)]
                     [(string=? "local" (tz dt)) (mktime v)]
                     [else (mktime v (tz dt))])))))
        (datetime year:   (+ 1900 (tm:year tm))
                  month:  (1+ (tm:mon  tm))
                  day:    (tm:mday tm)
                  hour:   (tm:hour tm)
                  minute: (tm:min  tm)
                  second: (tm:sec  tm))))))

;; Deprecated
;; datetime → time
;; Returns the local time from a date. Meaning that if the given datetime has
;; timezone info it's discarded and a local timestamp produced.
;; It's deprecated since the local time of a datetime can be in another date
;; than the original. which is fun...
(define-public (get-time dt)
  (get-time% (get-datetime dt)))


;;; UTIL

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
              29 28))
         (else (error "No month ~a (~a)" (month date) date))))

(define-public (days-in-year date)
  (if (leap-year? (year date))
      366 365))


(define-public (as-date date/-time)
  (if (date? date/-time)
      date/-time
      (get-date date/-time)))

(define-public (as-time date/-time)
  (if (datetime? date/-time)
      (get-time date/-time)
      (time)))

;;; EQUIALENCE

;; 2020-01-10 + 0-0-30 = 2020-02-09
;; 10 + 30 = 40                            ; day + day
;; 40 > 31                                 ; target days > days in month
;; 2020-02-00 + 0-0- (40 - 31)             ;
;; 2020-02-09

(define-public (date= a b)
  (and (= (year a) (year b))
       (= (month a) (month b))
       (= (day a) (day b))))

(define-public (time= a b)
  (and (= (hour a) (hour b))
       (= (minute a) (minute b))
       (= (second a) (second b))))

(define-public (datetime= a b)
  (and (date= (get-date a) (get-date b))
       (time= (get-time a) (get-time b))))

(define-many define-public
  (date=?) date=
  (time=?) time=
  (datetime=?) datetime=)

(define (date<% a b)
  (let ((ay (year a))
        (by (year b)))
    (if (= ay by)
        (let ((am (month a))
              (bm (month b)))
          (if (= am bm)
              (< (day a) (day b))
              (< am bm)))
        (< ay by))))

(define-public date<
  (match-lambda*
    [() #t]
    [(_) #t]
    [(first second . rest)
     (and (date<% first second)
          (apply date< second rest))]))

(define (date<=% a b)
  (or (date= a b)
      (date< a b)))

(define-public date<=
  (match-lambda*
    [() #t]
    [(_) #t]
    [(first second . rest)
     (and (date<=% first second)
          (apply date<= second rest))]))

(define-public (time< a b)
  (let ((ah (hour a))
        (bh (hour b)))
    (if (= ah bh)
        (let ((am (minute a))
              (bm (minute b)))
          (if (= am bm)
              (< (second a) (second b))
              (< am bm)))
        (< ah bh))))

(define-public (time<= a b)
  (or (time= a b)
      (time< a b)))

(define-public (datetime< a b)
  (if (date= (get-date a) (get-date b))
      (time< (get-time a) (get-time b))
      (date< (get-date a) (get-date b))))

(define-public (datetime<= a b)
  (if (date= (get-date a) (get-date b))
      (time<= (get-time a) (get-time b))
      (date<= (get-date a) (get-date b))))

(define-public (date/-time< a b)
  ;; (format (current-error-port) "~a < ~a = " a b)
  (let ((res
         (cond [(date= (as-date a) (as-date b))
                (time< (as-time a) (as-time b))]
               [(date< (as-date a) (as-date b))
                #t]
               [else #f])))
    ;; (format (current-error-port) "~a~%" res)
    res))

(define-many define-public
  (date<?) date<
  (date> date>?) (swap date<)
  (date<=?) date<=
  (date>= date>=?) (swap date<=)

  (time<?) time<
  (time> time>?) (swap time<)
  (time<=?) time<=
  (time>= time>=?) (swap time<=)

  (datetime<?) datetime<
  (datetime> datetime>?) (swap datetime<)
  (datetime<=?) datetime<=
  (datetime>= datetime>=?) (swap datetime<=)

  (date/-time<?) date/-time<
  (date/-time> date/-time>?) (swap date/-time<)
  (date/-time<= date/-time<=?) (negate date/-time>)
  (date/-time>= date/-time>=?) (negate date/-time<)
  )



;;; OPERATIONS

;; Base and change inverted to better work with fold in the exported date+
#;
(define (date+% change base)

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

(define-public (date-zero? date)
  (= 0 (year date) (month date) (day date)))

(define (date+%% change base)

  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      ;; (format (current-error-port) "1 ~s : ~s~%" target change)
      (if (>= (days-in-month target) (+ (day change) (day target)))
          ;; No date overflow, just add the change
          (values (set-> target (day = (+ (day change))))
                  (set-> change (day 0)))
          ;; Date (and possibly year) overflow
          (loop (if (= 12 (month target))
                    (set-> target
                           (year = (+ 1))
                           (month 1)
                           (day 1))
                    (set-> target
                           (month = (+ 1))
                           (day 1)))
                (set-> change (day = (- (1+ (- (days-in-month target) (day target))))))))))

  (define-values (month-fixed change**)
    (if (date-zero? change*)
        (values days-fixed change*)
     (let loop ((target days-fixed) (change change*))
       ;; (format (current-error-port) "2 ~s : ~s~%" target change)
       (if (< 12 (+ (month change) (month target)))
           ;; if we overflow into the next year
           (loop (set-> target
                        (year = (+ 1))
                        (month 1))
                 (set (month change) = (- (- 13 (month target)))))

           ;; if we don't overflow our date
           (values (set (month target) = (+ (month change)))
                   (set (month change) 0))

           ))))

  ;; change** should here should have both month and date = 0

  (set (year month-fixed) = (+ (year change**))))

(define (date+% change base)

  (when (or (negative? (year change))
            (negative? (month change))
            (negative? (day change)))
    (error "Change can't be negative"))

  (when (or (negative? (month base))
            (negative? (day base)))
    (error "Base month or day can't be negative"))

  (date+%% change base)
  )

;; @var{base} MUST be a valid real date. all rest arguments can however
;; be "invalid" dates, such as 0000-00-10
(define-public (date+ base . rest)
  (fold date+% base rest))

(define (date-%% change base)
  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (day change) (day target))
          (let ((new-change (set (day change) = (- (day target)))))
            (loop (if (= 1 (month target))
                      (set-> target
                             (year = (- 1))
                             (month 12)
                             (day 31)              ; days in december
                             )
                      (set-> target
                             (month = (- 1))
                             (day (days-in-month (set (month target) = (- 1))))))
                  new-change))
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

  (set (year month-fixed) = (- (year change**))))

(define (date-% change base)

  (when (or (negative? (year change))
            (negative? (month change))
            (negative? (day change)))
    (error "Change can't be negative"))

  (when (or (negative? (month base))
            (negative? (day base)))
    (error "Base month or day can't be negative"))

  (date-%% change base)
  )

;;; Only use this with extreme caution
(define-public (date- base . rest)
  (fold date-% base rest))

;;; time

;; time x time → time x int
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

;;; PLUS
(define-public (time+ base . rest)
  (let ((sum 0))
    (let ((time (fold (lambda (next done)
                        (let* ((next-time rem (time+% done next)))
                          (mod! sum = (+ rem))
                          next-time))
                      base rest)))
      (values time sum))))

(define (time-% base change)

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


  (if (>= (hour minute-fixed) (hour change))
      (values (set (hour minute-fixed) = (- (hour change)))
              0)
      (values (set (hour minute-fixed) 0)
              (- (hour change) (hour minute-fixed)))))

(define-public (time- base . rest)
  (let ((sum 0))
    (let ((time (fold (lambda (next done)
                        (let* ((next-time rem (time-% done next)))
                          (mod! sum = (+ rem))
                          next-time))
                      base rest)))
      (values time sum))))


;;; DATETIME

(define-public (datetime+ base change)
  (let* ((time overflow (time+ (get-time base) (get-time change))))
    (datetime date: (date+ (get-date base)
                           (get-date change)
                           (date day: overflow))
              time: time)))

(define (datetime->srfi-19-date date)
  ((@ (srfi srfi-19) make-date)
    0
    (second (get-time date))
    (minute (get-time date))
    (hour (get-time date))
    (day (get-date date))
    (month (get-date date))
    (year (get-date date))
    0                                   ; TODO TZ
    ))

(define (srfi-19-date->datetime o)
  (let ((y ((@ (srfi srfi-19) date-year)   o)))
    ;; TODO find better way to translate from 1970 to 0, since this WILL
    ;; cause problems sooner or later.
   (datetime year:   (if (= 1970 y) 0 y)
             month:  (let ((m ((@ (srfi srfi-19) date-month) o)))
                       (if (and (= 1970 y) (= 1 m)) 0 m))
             day:    (let ((d ((@ (srfi srfi-19) date-day) o)))
                       (if (and (= 1970 y) (= 1 d)) 0 d))
             hour:   ((@ (srfi srfi-19) date-hour)   o)
             minute: ((@ (srfi srfi-19) date-minute) o)
             second: ((@ (srfi srfi-19) date-second) o)
             )))

;;; the *-difference procedures takes two actual datetimes.
;;; date- instead takes a date and a delta (but NOT an actual date).

(define-public (datetime-difference end start)
  (let ((t
         ((@ (srfi srfi-19) time-difference)
          ((@ (srfi srfi-19) date->time-utc) (datetime->srfi-19-date end))
          ((@ (srfi srfi-19) date->time-utc) (datetime->srfi-19-date start)))))
    ((@ (srfi srfi-19) set-time-type!) t (@ (srfi srfi-19) time-utc))
    (srfi-19-date->datetime ((@ (srfi srfi-19) time-utc->date) t 0))))  ; TODO tz offset

(define-public (date-difference end start)
  (get-date (datetime-difference (datetime date: end)
                                 (datetime date: start))))


;;; Parsers for vcomponent usage

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
        second: (s->n str 4 6)))

;;; TODO when parsing recurrence rules sometimes I think this is
;;; sent regular dates
(define*-public (parse-datetime str optional: tz)
  (let* (((datestr timestr) (string-split str #\T)))
    (datetime date: (parse-date datestr)
              time: (parse-time timestr)
              tz: tz)))


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
    (let ((hour (string->number hour))
          (minute (string->number minute))
          (second (string->number second)))
      `(time hour: ,hour minute: ,minute second: ,second))))

(define (parse-datetime% str)
  (let* (((date time) (string-split str #\T)))
    (when (string= "Z" (string-take-right str 1))
      (set! time (string-drop-right time 1)))
    `(datetime date: ,(parse-date% date)
               time: ,(parse-time% time)
               tz: ,(and (string= "Z" (string-take-right str 1))
                         'Z))))

(define (date-reader chr port)
  (unread-char chr port)
  (let ((line (symbol->string (read port))))
    (cond [(string-contains line "T") (parse-datetime% line)]
          [(string-contains line ":") (parse-time% line)]
          [(string-contains line "-") (parse-date% line)])))

(read-hash-extend #\0 date-reader)
(read-hash-extend #\1 date-reader)
(read-hash-extend #\2 date-reader)

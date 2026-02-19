;;; Commentary:
;;; Base arithmetic operations for zone un-aware datetime objects.
;;; Code:
(define-module (datetime arithmetic)
  :use-module (datetime core)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util type)
  :export (
           date+ date-
           time+ time-
           datetime+ datetime-
           date-difference
           datetime-difference
           ))


;; NOTE +1 month is weird for late days in a month.
;; is the last of january +1 month the last of february,
;; or a few days into march? It's at least not the 31 of
;; February, as the code is currently written.
;; (date+ #2020-01-31 #0000-01-00) ; => 2020-02-31
(define (date+%% change base)

  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (days-in-month target) (+ (day change) (day target)))
          ;; No date overflow, just add the change
          (values (-> target (day (+ (day target)
                                     (day change))))
                  (day change 0))
          ;; Date (and possibly year) overflow
          (loop (if (= 12 (month target))
                    (-> (modify target year* 1+)
                        (month 1)
                        (day 1))
                    (-> (modify target month* 1+)
                        (day 1)))
                ;; How did this ever work‽
                (modify change day*
                        (lambda (d) (- d
                                  (- (day target))
                                  (days-in-month target)
                                  1)))))))

  (define-values (month-fixed change**)
    (if (date-zero? change*)
        (values days-fixed change*)
     (let loop ((target days-fixed) (change change*))
       (if (< 12 (+ (month change) (month target)))
           ;; if we overflow into the next year
           (loop (-> (modify target year* 1+)
                     (month 1))
                 (modify change month*
                         (lambda (d) (+ d (month target) -13))))
           ;; if we don't overflow our date
           (values (modify target month* (lambda (d) (+ d (month change))))
                   (month change 0))

           ))))

  ;; change** should here should have both month and date = 0

  (year month-fixed (+ (year month-fixed) (year change**))))

(define (date+% change base)

  (when (or (negative? (year change))
            (negative? (month change))
            (negative? (day change)))
    (scm-error 'misc-error "date+%" "Negative change ~a invalid (base=~a)"
               (list change base)
               #f))

  (unless (and (< 0 (month base))
               (< 0 (day base)))
    (scm-error 'misc-error "date+%"
           "~a needs day and month to be at least one"
           (list base)
           #f))

  (date+%% change base))

;; @var{base} MUST be a valid real date. all rest arguments can however
;; be "invalid" dates, such as 0000-00-10
(define (date+ base . rest)
  (fold date+% base rest))

(define (date-%% change base)
  (define-values (days-fixed change*)
    (let loop ((target base) (change change))
      (if (>= (day change) (day target))
          (let ((new-change (modify change day* (lambda (d) (- d (day target))))))
            (loop (if (= 1 (month target))
                      (-> (modify target year* 1-)
                          (month 12)
                          (day 31)              ; days in december
                          )
                      (let ((nm (modify target month* 1-)))
                        (day nm (days-in-month nm))))
                  new-change))
          (values (modify target day* (lambda (d) (- d (day change))))
                  (day change 0)))))

  (define-values (month-fixed change**)
    (let loop ((target days-fixed) (change change*))
      (if (>= (month change) (month target))
          (loop (-> (modify target year* 1-)
                    (month 12))
                (modify change month* (lambda (d) (- d (month target)))))
          (values (modify target month* (lambda (d) (- d (month change))))
                  (month change 0)))))

  ;; change** should here should have both month and date = 0

  (modify month-fixed year* (lambda (d) (- d (year change**)))))

(define (date-% change base)

  (when (or (negative? (year change))
            (negative? (month change))
            (negative? (day change)))
    (scm-error 'misc-error "date-%" "Negative change ~a invalid (base=~a)"
           (list change base)
           #f))

  (when (or (negative? (month base))
            (negative? (day base)))
    (scm-error 'misc-error "date-%"
           "~a needs day and month to be at least one"
           (list base)
           #f))

  (date-%% change base)
  )

;;; Only use this with extreme caution
(define (date- base . rest)
  (fold date-% base rest))

;;; time

;; overflow is number of days above
;; time x time → time x int
(define (time+% base change)

  ;; while (day base) > (days-in-month base)
  ;;     month++; days -= (days-in-month base)
  (define second-fixed
    (let loop ((target (modify base second* (lambda (d) (+ d (second change))))))
      (if (>= (second target) 60)
          (loop (-> target
                    (modify minute* 1+)
                    (modify second* (lambda (d) (- d 60)))))
          target)))

  ;; while (month base) > 12
  ;;     year++; month -= 12
  (define minute-fixed
    (let loop ((target (modify second-fixed minute* (lambda (d) (+ d (minute change))))))
      (if (>= (minute target) 60)
          (loop (-> target
                    (modify hour* 1+)
                    (modify minute* (lambda (d) (- d 60)))))
          target)))

  (define hour-almost-fixed (modify minute-fixed hour* (lambda (d) (+ d (hour change)))))

  (if (<= 24 (hour hour-almost-fixed))
      (let ((div remainder (floor/ (hour hour-almost-fixed) 24)))
        (values (hour hour-almost-fixed remainder) div))
      (values hour-almost-fixed 0)))

;;; PLUS
(define (time± proc)
  (lambda (base . rest)
   (let loop ((time-accumulated base) (overflow 0) (remaining rest))
     (if (null? remaining)
         (values time-accumulated overflow)
         (let ((next-time rem (proc time-accumulated (car remaining))))
           (loop next-time (+ overflow rem) (cdr remaining)))))))

(define time+ (time± time+%))

;; time, Δtime → time, hour
(define (time-% base change)

  (define-values (second-fixed change*)
    (let loop ((target base) (change change))
      (if (> (second change) (second target))
          (loop (-> (modify target minute* 1-)
                    (second 60))
                (modify change second* (lambda (d) (- d (second target)))))
          (values (modify target second* (lambda (d) (- d (second change))))
                  (second change 0)))))

  (define-values (minute-fixed change**)
    (let loop ((target second-fixed) (change change*))
      (if (> (minute change) (minute target))
          (loop (-> (modify target hour* 1-)
                    (minute 60))
                (modify change minute* (lambda (d) (- d (minute target)))))
          (values (modify target minute* (lambda (d) (- d (minute change))))
                  (minute change 0)))))

  (if (>= (hour minute-fixed) (hour change**))
      (values (modify minute-fixed hour* (lambda (d) (- d (hour change**)))) 0)
      (let ((diff (- (hour minute-fixed)
                     (hour change**))))
        (values (hour minute-fixed (modulo diff 24))
                (abs (floor (/ diff 24)))))))

;; Goes backwards from base, returning the two values:
;; the new time, and the number of days back we went.
;; Note that neither time+ or time- can return a time
;; component greater than 24h, but nothing is stoping
;; a user from creating them manually.
;; @lisp
;; (time- #10:00:00 #09:00:00) ; => 01:00:00 => 0
;; (time- #03:00:00 #07:00:00) ; => 20:00:00 => 1
;; (time- #10:00:00 (time hour: 48)) ; => 10:00:00 => 2
;; (time- #10:00:00 (time hour: (+ 48 4))) ; => 06:00:00 => 2
;; @end lisp
(define time- (time± time-%))


;;; DATETIME


(define (datetime+ base change)
  ;; Note that this is timezone unaware (by design)
  ;; This means that change is added to date completely ignoring timezones.
  ;; This means that +1 day and +24 hours are identical here
  (typecheck (tz change) false?)
  (let ((new-time overflow (time+ (datetime-time base) (datetime-time change))))
    (-> base
        (modify date*
                (lambda (d) (date+ d
                              (datetime-date change)
                              (date day: overflow))))
        (set time* new-time))))

(define (datetime- base change)
  ;; Note that this is timezone unaware (by design)
  ;; This means that change is added to date completely ignoring timezones.
  ;; This means that +1 day and +24 hours are identical here
  (typecheck (tz change) false?)
  (let ((new-time underflow (time- (datetime-time base) (datetime-time change))))
    (-> base
        (modify date*
                (lambda (d) (date- d
                              (datetime-date change)
                              (date day: underflow))))
        (set time* new-time))))

;;; the *-difference procedures takes two actual datetimes.
;;; date- instead takes a date and a delta (but NOT an actual date).

;; Works on 0-based dates. So the last of January 2020 becomes
;; 2020-00-30
(define (date-difference% b a)
  ;; #2020-01-01 #2020-00-26 → #2020-00-06 #2020-00-00
  (define-values (b* a*)
    (let loop ((b b) (a a))
      (if (> (day a) (day b))
          (let ((new-a (day a (- (day a) (day b) 1))))
            (loop (if (= 0 (month b))
                      (-> (modify b year* 1-)
                          (month 11)
                          (day 30)   ; Last day in december
                          )
                      (-> (modify b month* 1-)
                          (day (1- (days-in-month b))))) ; last in prev month
                  new-a))
          ;; elif (> (day b) (day a))
          (values (day b (- (day b) (day a)))
                  (day a 0)))))


  ;; (day a*) should be 0 here.

  (define-values (b** a**)
    (let loop ((b b*) (a a*))
      (if (> (month a) (month b))
          (loop (-> (modify b year* 1-)
                    (month 11))
                (modify a month* (lambda (d) (- d 1 (month b)))))
          ;; elif (> (month b) (month a))
          (values (modify b month* (lambda (d) (- d (month a))))
                  (month a 0)))))

  ;; a** should here should have both month and date = 0

  (year b** (- (year b**) (year a**))))



;; Earlier date after later date to have same semantics as subtraction
(define (date-difference later-date earlier-date)
  (when (date< later-date earlier-date)
    (scm-error 'misc-error "date-difference"
               "The earlier of the two dates must come after. later-date: ~a, earlier-date: ~a"
               (list later-date earlier-date) #f))
  (when (or (negative? (month later-date))
            (negative? (day   later-date))
            (negative? (month earlier-date))
            (negative? (day   earlier-date)) )
    (scm-error 'misc-error "date-difference"
           "~a or ~a contains negative months or days"
           (list earlier-date later-date)
           #f))

  (let ((proc (lambda (d) (-> d
                         (modify month* 1-)
                         (modify day* 1-)))))
    (date-difference% (proc later-date)
                      (proc earlier-date))))


;; NOTE, this is only properly defined when end is greater than start.
(define (datetime-difference end start)
  (unless (or (equal? #f (tz start) (tz end))
              (equal? "UTC" (tz start) (tz end)))
    (scm-error
     'wrong-type-arg "datetime-difference"
     "Datetime difference only defined for UTC or zoneless datetimes. Got start: ~s, end: ~s"
     (list start end) #f))

  (let ((fixed-time overflow (time- (datetime-time end)
                                    (datetime-time start))))
    (datetime date: (date-difference (date- (datetime-date end)
                                            (date day: overflow))
                                     (datetime-date start))
              time: fixed-time)))

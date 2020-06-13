(define-module (vcomponent duration)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (datetime)
  :use-module (ice-9 peg)
  :use-module (ice-9 match)
  :use-module (srfi srfi-9 gnu)
  :use-module (srfi srfi-1)
  :export (duration parse-duration))

(define-immutable-record-type <duration>
  (make-duration sign week day dur-time)
  duration?
  (sign duration-sign)
  (week duration-week)
  (day duration-day)
  (dur-time duration-time))

(define* (duration
          key: (sign '+)
          week day time)
  (when (and week (or day time))
    (error "Can't give week together with day or time"))
  (make-duration sign week day time))

(define-peg-pattern number all (+ (range #\0 #\9)))

(define-peg-pattern time-pattern body
  (and (ignore "T")
       (and (capture (and number "H"))
            (? (and (capture (and number "M"))
                    (? (capture (and number "S"))))))))

(define-peg-pattern dur-pattern body
  (and (capture (? (or "+" "-")))
       (and "P"
            (or (capture (and number "W"))
                (or (capture (and (and number "D")
                                  (? time-pattern)))
                    (capture time-pattern))))))

(define (parse-duration str)
  (let ((m (match-pattern dur-pattern str)))
    (unless m
      (error "~a doesn't appar to be a duration" str))

    (unless (= (peg:end m) (string-length str))
      (warning "Garbage at end of duration"))

    (let* ((tree (peg:tree m))
           (sign (case (string->symbol (car tree))
                   [(+ -) => identity]
                   [(P) '+]))
           (lst (concatenate
                 (map (match-lambda
                        [(('number num) type)
                         (let ((n (string->number num)))
                           (case (string->symbol type)
                             [(W) `(week: ,n)]
                             [(D) `(day:  ,n)]
                             [(H) `(hour: ,n)]
                             [(M) `(minute: ,n)]
                             [(S) `(second: ,n)]
                             [else (error "Invalid key")]))]
                        [#\T '()])
                      (cadr (member "P" tree))))))
      (apply duration
             (cons* sign: sign
                    (let loop ((rem lst))
                      (if (null? rem)
                          '()
                          (if (eqv? hour: (car rem))
                              (list time: (apply time rem))
                              (cons* (car rem) (cadr rem)
                                     (loop (cddr rem)))))))))))

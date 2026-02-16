(define-module (vcomponent type duration)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (hnh util object)
  :use-module (hnh util serialize)
  :use-module (hnh util type)
  :use-module (datetime)
  :use-module (ice-9 peg)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module (vcomponent media-type parse-error)
  :export (duration
           duration?

           duration-sign

           string->duration
           duration->string

           duration->datetime

           ;; NOTE Should the -week and -datetime stuff actually be
           ;; exported? They require extra care, since the *apparent*
           ;; duration type is a union of these two.
           duration-week duration-week?
           duration-datetime duration-datetime?

           duration-week-count duration-week-count*
           duration-day        duration-day*
           duration-time       duration-time*
           ))

(define-type (duration-week serializer: (lambda (o)
                                          `(duration
                                            sign: ,(serialize (duration-week-sign o))
                                            week: ,(duration-week-count o))))
  (duration-week-sign  keyword: sign type: (memv '(+ -)))
  (duration-week-count keyword: week type: integer?))

(define-type (duration-datetime serializer: (lambda (o)
                                              `(duration
                                                sign: ,(serialize (duration-datetime-sign o))
                                                day: ,(duration-day o)
                                                time: ,(duration-time o))))
  (duration-datetime-sign keyword: sign type: (memv '(+ -)))
  (duration-day  keyword: day  default: #f
                 type: (or false? integer?))
  (duration-time keyword: time default: #f
                 type: (or false? time?)))

(define (duration? x)
  (or (duration-week? x)
      (duration-datetime? x)))

(define (duration-sign duration)
  (typecheck duration duration?)
  ((cond ((duration-week? duration) duration-week-sign)
         ((duration-datetime? duration) duration-datetime-sign))
   duration))

;;; TODO duration-sign* lens

(define* (duration
          key: (sign '+)
          week day time)
  (when (and week (or day time))
    (scm-error 'misc-error "duration"
               "Can't give week together with day or time"
               #f #f))
  (if week
      (duration-week sign: sign week: week)
      (duration-datetime
       sign: sign
       day: day
       time: time)))


(define (duration->string duration)
  (with-output-to-string
    (lambda ()
      (unless (eq? '+ (duration-sign duration))
        (display (duration-sign duration)))
      (display "P")
      (cond ((duration-week? duration)
             (format #t "~aW" (duration-week-count duration)))
            ((duration-datetime? duration)
             (awhen (duration-day duration) (format #t "~aD" it))
             (awhen (duration-time duration)
                    (display "T")
                    ;; if any non-zero,
                    (unless (= 0 (hour it) (minute it) (second it))
                      (format #t "~aH" (hour it))
                      (unless (= 0 (minute it) (second it))
                        (format #t "~aM" (minute it))
                        (unless (= 0 (second it))
                          (format #t "~aS" (second it)))))))))))


(define-peg-pattern number all (+ (range #\0 #\9)))

(define-peg-pattern time-pattern body
  (and (ignore "T")
       (and (? (capture (and number "H")))
            (? (and (? (capture (and number "M")))
                    (? (capture (and number "S"))))))))

(define-peg-pattern dur-pattern body
  (and (capture (? (or "+" "-")))
       (and "P"
            (or (capture (and number "W"))
                (or (capture (and (and number "D")
                                  (? time-pattern)))
                    (capture time-pattern))))))

(define (string->duration str)
  (let ((m (match-pattern dur-pattern str)))
    (unless m
      (raise-calendar-parse-error
       type: 'DURATION
       value: str))

    (unless (= (peg:end m) (string-length str))
      (raise-calendar-parse-error
       type: 'DURATION
       value: str
       msg: "Garbage at end of duration"))

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
                             [else (unreachable
                                    "string->duration"
                                    "Invalid key ~a"
                                    type)]))]
                        [a
                         (unreachable
                          "string->duration"
                          "~s not on expected form ((number <num>) type)"
                          (list a))])
                      (context-flatten (lambda (x) (and (pair? (car x))
                                                   (eq? 'number (caar x))))
                      (cdr (member "P" tree)))
                      ))))
      (apply duration
             (cons* sign: sign
                    (let loop ((rem lst))
                      (if (null? rem)
                          '()
                          ;; NOTE a potentially prettier way would be
                          ;; to capture the T above, and use that as
                          ;; the delimiter for the time.
                          (if (memv (car rem) '(hour: minute: second:))
                              (list time: (apply time rem))
                              (cons* (car rem) (cadr rem)
                                     (loop (cddr rem)))))))))))



(define (duration->datetime duration)
  (typecheck duration duration?)
  (values (duration-sign duration)
          (cond ((duration-week? duration)
                 (datetime day: (* 7 (duration-week-count duration))))
                ((duration-datetime? duration)
                 (datetime day: (or (duration-day duration) 0)
                           time: (duration-time duration)))
                (else (unreachable "duration->datetime" "Bad duration type: ~s"
                                   (list duration))))))

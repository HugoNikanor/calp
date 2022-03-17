(define-module (vcomponent duration)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
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
    (scm-error 'misc-error "duration"
               "Can't give week together with day or time"
               #f #f))
  (make-duration sign week day time))


(define-public (format-duration duration)
  (with-output-to-string
    (lambda ()
      (unless (eq? '+ (duration-sign duration))
        (display (duration-sign duration)))
      (display "P")
      (aif (duration-week duration)
           (format #t "~aW" it)
           (begin
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

(define (parse-duration str)
  (let ((m (match-pattern dur-pattern str)))
    (unless m
      (scm-error 'parse-error "parse-duration"
                 "~s doesn't appar to be a duration"
                 (list str)
                 #f))

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
                             [else (scm-error 'misc-error "parse-duration"
                                              "Invalid key ~a" type #f)]))]
                        [a
                         (scm-error 'misc-error "parse-duration"
                                    "~s not on expected form ((number <num>) type)"
                                    (list a) #f)])
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

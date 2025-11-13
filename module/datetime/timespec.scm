;;; Commentary:
;; Datatype for holding timechanges and time offesets.
;; Used both for timespecs from the TZ-database, and for UTC-OFFSET from RFC5545.
;;; Code:

(define-module (datetime timespec)
  :use-module ((hnh util) :select (unless))
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 regex)
  :export (timespec
           timespec?
           timespec->string
           timespec-time timespec-time*
           timespec-sign timespec-sign*
           timespec-type timespec-type*

           timespec-zero
           timespec+
           timespec-negate
           datetime-timespec-add
           parse-time-spec
           ))


;; timespec as defined by the TZ-database
;; also used UTC-OFFSET defined by RFC5545. Then type should equal #\z
;; and be ignored.


(define-type (timespec
              constructor:
              (lambda (constructor type-check)
                (lambda (time sign type)
                  (type-check time sign type)
                  (constructor time sign type)))
              serializer:
              (lambda (r)
                `(timespec ,(serialize (timespec-time r))
                           ,(serialize (timespec-sign r))
                           ,(serialize (timespec-type r)))))
  (timespec-time type: time?)
  (timespec-sign type: (memv '(+ -)))
  ;; types:
  ;; w - wall clock time (local time)
  ;; s - standard time without daylight savings adjustments
  ;;     This means what the local time would be, if dayligts saving
  ;;     wasn't applied. So for Europe/Stockholm, wall time would
  ;;     alternate between +01:00 and +02:00, while standard time would
  ;;     always be +01:00 (as of 2024)
  ;; u, g, z - Universal time, all three are synonyms due to historical reasons
  (timespec-type type: (or false? (memv '(standard daylight wall utc)))))

(define (timespec->string timespec)
  (typecheck timespec timespec?)
  (format #f "~a~a~a"
          (timespec-sign timespec)
          (time->string (timespec-time timespec))
          (case (timespec-type timespec)
            ((standard) "s")
            ((daylight) "d")
            ((wall) "w")
            ((utc) "u")
            ((#f) "")
            (else
             ;; Unknown type, emit this as an error
             "!"))))

(define (timespec-zero)
  (timespec (time) '+ #f))

(define (timespec+ . timespecs)
  #;
  (let ((types (map timespec-type timespecs)))
    (unless (apply eqv? types)
      (warning "Adding timespecs of differing types: ~s"
               types)))

  (define-values (sum-time sum-overflow)
    (car+cdr
     (fold (lambda (ts p)
             (define-values (sum-time sum-overflow) (car+cdr p))
             (case (timespec-sign ts)
               ((+) (let ((t o (time+ sum-time (timespec-time ts))))
                      (cons t (+ sum-overflow o))))
               ((-) (let ((t o (time- sum-time (timespec-time ts))))
                      (cons t (- sum-overflow o))))
               (else (scm-error 'misc-error "timespec+"
                                "Invalid timespec sign: ~s"
                                (list (timespec-sign ts))
                                #f))))
           (cons (time) 0)
           timespecs)))

  ;; Check negative, since we want to treat 0 as "positive"
  (timespec
   (if (negative? sum-overflow)
       (seconds->time
        (modulo
         (- (time->seconds sum-time))
         (* -24 60 60 sum-overflow)))
       (modify sum-time hour*
               (lambda (h) (+ h (* 24 sum-overflow)))))
   (if (negative? sum-overflow) '- '+)
   (if (null? timespecs)
       #f (timespec-type (car timespecs)))))



(define (timespec-negate ts)
  (modify ts timespec-sign*
          (lambda (s)
            (if (eq? s '+) '- '+))))


;;; Add a timespec to a datetime
(define (datetime-timespec-add dt ts)
  ((case (timespec-sign ts)
     ((+) datetime+)
     ((-) datetime-))
   dt (datetime time: (timespec-time ts))))


;;; [+-]?\d\d:\d\d:\d\d[swugz]
;; "+10:20:30.13"
(define-once timespec-rx
  (make-regexp "^([+-])?([0-9]{1,2})(:([0-9]{2}))?(:([0-9]{2}))?([.]([0-9]*))?([swugzd])?$"))
(define (parse-time-spec string)
  (cond ((string=? string "-")
         (timespec (time) '+ #f))
        ((regexp-exec timespec-rx string)
         => (lambda (m)
              (timespec
               (time hour: (string->number (match:substring m 2))
                     minute: (cond ((match:substring m 4) => string->number)
                                   (else 0))
                     second: (cond ((match:substring m 6) => string->number)
                                   (else 0)))
               (cond ((match:substring m 1) => string->symbol)
                     (else '+))
               (cond ((match:substring m 9)
                      => (lambda (s) (case (string-ref s 0)
                                  ((#\w) 'wall)
                                  ((#\s) 'standard)
                                  ((#\d) 'daylight)
                                  ((#\u #\g #\z) 'utc))))
                     (else #f)))))

        (else (scm-error 'misc-error "parse-time-spec"
                         "String not parsable as a timespec: ~s"
                         (list string) #f))))

;;; Commentary:
;; Datatype for holding timechanges and time offesets.
;; Used both for timespecs from the TZ-database, and for UTC-OFFSET from RFC5545.
;;
;; TODO rewrite this to be a duration and a "type" of time tuple instead, updating usage where applicable
;;; Code:

(define-module (datetime timespec)
  :use-module ((hnh util) :select (unless awhen))
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util serialize)
  :use-module (hnh util lens)
  :use-module (datetime core)
  :use-module (datetime arithmetic)
  :use-module (datetime duration)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 regex)
  :use-module (ice-9 format)
  :use-module (ice-9 curried-definitions)
  :export (timespec
           timespec?
           timespec->string

           timespec-time timespec-time*
           timespec-sign timespec-sign*

           timespec-value timespec-value*
           timespec-type timespec-type*

           timespec+
           timespec-negate
           datetime-timespec-add
           parse-time-spec

           timespec->integer integer->timespec
           ))


(define (sgn x)
  (if (zero? x) x
      (/ x (abs x))))


;; timespec as defined by the TZ-database
;; also used UTC-OFFSET defined by RFC5545. Then type should equal #\z
;; and be ignored.

(define-type (timespec
              constructor:
              (lambda (constructor type-check)
                (lambda* (tm optional: (sign '+) type)
                  (define v (* (if (eq? '- sign) -1 1)
                               (time->seconds tm)))
                  (type-check v type)
                  (constructor v type)))
              serializer:
              (lambda (r)
                `(timespec ,(seconds->time (abs (timespec-value r)))
                           ,@(if (and (positive? (timespec-value r))
                                      (not (timespec-type r)))
                                 '()
                                 `(,(serialize '-)))
                           ,@(awhen (timespec-type r)
                                    (list (serialize it))))))
  ;; (timespec-time type: time?)
  ;; (timespec-sign type: (memv '(+ -)))
  (timespec-value type: exact-integer?)
  ;; types:
  ;; w - wall clock time (local time)
  ;; s - standard time without daylight savings adjustments
  ;;     This means what the local time would be, if dayligts saving
  ;;     wasn't applied. So for Europe/Stockholm, wall time would
  ;;     alternate between +01:00 and +02:00, while standard time would
  ;;     always be +01:00 (as of 2024)
  ;; u, g, z - Universal time, all three are synonyms due to historical reasons
  (timespec-type type: (or false? (memv '(standard daylight wall utc)))))

;;; DEPRECATED
(define ((timespec-time* ts) f)
  (modify ts timespec-value*
          (lambda (v) (* (sgn v)
                    (time->seconds (f (seconds->time (abs v))))))))

;;; DEPRECATED
(define timespec-time
  (case-lambda ((ts)   (get ts timespec-time*))
               ((ts v) (set ts timespec-time* v))))

;;; DEPRECATED
(define ((timespec-sign* ts) f)
  (modify ts timespec-value*
          (lambda (v) (if (eq? '- (f (if (negative? v) '- '+)))
                     (* -1 (abs v))
                     (abs v)))))

;;; DEPRECATED
(define timespec-sign
  (case-lambda ((ts)   (get ts timespec-sign*))
               ((ts v) (set ts timespec-sign* v))))


(define* (timespec->string timespec
                           optional: (precision 'h)
                           key: (delimiter ":"))
  (typecheck timespec timespec?)
  (typecheck precision (memv '(h m s)))

  (with-output-to-string
    (lambda ()
      (define t (timespec-value timespec))
      (display (if (negative? t)
                   "-" ""))
      (let* ((h r (floor/ (abs t) 3600))
             (m s (floor/ r 60)))
        (format #t "~2'0d" h)
        (when (or (memv precision '(m s))
                  (not (= 0 m s)))
          (format #t "~a~2'0d" delimiter m)
          (when (or (memv precision '(s))
                    (not (= 0 s)))
            (format #t "~a~2'0d" delimiter s))))
      ;; Print milis here once we store them
      (display
       (case (timespec-type timespec)
         ((standard) "s")
         ((daylight) "d")
         ((wall) "w")
         ((utc) "u")
         ((#f) "")
         (else
          ;; Unknown type, emit this as an error
          "!"))))))

(define (timespec+ . timespecs)
  #;
  (let ((types (map timespec-type timespecs)))
    (unless (apply eqv? types)
      (warning "Adding timespecs of differing types: ~s"
               types)))

  (define sum (apply + (map timespec-value timespecs)))

  ;; Check negative, since we want to treat 0 as "positive"
  (timespec
   (seconds->time (abs sum))
   (if (negative? sum) '- '+)
   (if (null? timespecs)
       #f (timespec-type (car timespecs)))))



(define (timespec-negate ts)
  (modify ts timespec-value*
          (lambda (v) (* -1 v))))


;;; Add a timespec to a datetime
(define (datetime-timespec-add dt ts)
  (datetime+ dt (seconds->duration (timespec-value ts))))


;; "+10:20:30.13"
;; suffix   = "s" / "w" / "u" / "g" / "z" / "d"
;; hour     = 1*2DIGIT
;; minute   = 2DIGIT
;; second   = 2DIGIT
;; milis    = *DIGIT
;; timespec = ["+" / "-"] hour [":" minute [":" second ["." millis]]] [suffix]
(define-once timespec-rx
  (make-regexp "^([+-])?([0-9]{1,2})(:([0-9]{2}))?(:([0-9]{2}))?([.]([0-9]*))?([swugzd])?$"))
(define (parse-time-spec string)
  (cond ((string=? string "-")
         (timespec (time)))
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



(define timespec->integer timespec-value)

(define (integer->timespec i)
  (timespec (seconds->time (abs i))
            (if (positive? i)
                '+ '-)))

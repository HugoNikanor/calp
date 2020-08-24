;;; Commentary:
;; Datatype for holding timechanges and time offesets.
;; Used both for timespecs from the TZ-database, and for UTC-OFFSET from RFC5545.
;;; Code:

(define-module (datetime timespec)
  :use-module (calp util)
  :use-module (calp util exceptions)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9 gnu)
  )


;; timespec as defined by the TZ-database
;; also used UTC-OFFSET defined by RFC5545. Then type should equal #\z
;; and be ignored.
(define-immutable-record-type <timespec> ; EXPORTED
  (make-timespec timespec-time sign type)
  timespec?
  (timespec-time timespec-time)         ; <time>
  (sign timespec-sign)                  ; '+ | '-
  ;; types:
  ;; w - wall clock time (local time)
  ;; s - standard time without daylight savings adjustments
  ;; u, g, z - Universal time
  (type timespec-type))                 ; char

(export make-timespec timespec? timespec-time timespec-sign timespec-type)

(define-public (timespec-zero)
  (make-timespec (time) '+ #\w))

(define-public (timespec-add . specs)
  (unless (apply eqv? (map timespec-type specs))
    (warning "Adding timespecs of differing types"))

  (reduce (lambda (spec done)
            (cond
             ;; - -
             ;; + +
             [(eq? (timespec-sign done)
                   (timespec-sign spec))
              (set (timespec-time done) = (time+ (timespec-time spec)))]
             ;; - +
             [(and (eq? '- (timespec-sign done))
                   (eq? '+ (timespec-sign spec)))
              (let ((time-a (timespec-time done))
                    (time-b (timespec-time spec)))
                (if (time< time-a time-b)
                    (make-timespec (time- time-b time-a)
                                   '+ (timespec-type done))
                    (set (timespec-time done) (time- time-b))))]
             ;; + -
             [(and (eq? '+ (timespec-sign done))
                   (eq? '- (timespec-sign spec)))
              (let ((time-a (timespec-time done))
                    (time-b (timespec-time spec)))
                (if (time< time-a time-b)
                    (make-timespec (time- time-b time-a)
                                   '- (timespec-type done))
                    (set (timespec-time done) (time+ time-b))))]))
          (timespec-zero)
          specs))


(define (parse-time string)
  (apply (lambda* (hour optional: (minute "0") (second "0"))
           (time hour: (string->number hour)
                 minute: (string->number minute)
                 ;; discard sub-seconds
                 second: (string->number (car (string-split second #\.)))))
         (string-split string #\:)))


(define*-public (parse-time-spec
                 string optional: (suffixes '(#\s #\w #\u #\g #\z)))
  (let* ((type string
          (cond [(string-rindex string (list->char-set suffixes))
                 => (lambda (idx)
                      (values (string-ref string idx)
                              (substring string 0 idx)))]
                [else (values #\w string)])))
    (cond [(string=? "-"  string)
           (make-timespec (time) '+ type)]
          [(string-prefix? "-" string)
           (make-timespec (parse-time (string-drop string 1))
                          '- type)]
          [else
           (make-timespec (parse-time string)
                          '+ type)])))

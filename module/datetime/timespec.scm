;;; Commentary:
;; Datatype for holding timechanges and time offesets.
;; Used both for timespecs from the TZ-database, and for UTC-OFFSET from RFC5545.
;;; Code:

(define-module (datetime timespec)
  :use-module ((hnh util) :select (unless))
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :export (make-timespec
           timespec?
           timespec-time
           timespec-sign
           timespec-type

           timespec-zero
           timespec-add
           parse-time-spec
           ))


;; timespec as defined by the TZ-database
;; also used UTC-OFFSET defined by RFC5545. Then type should equal #\z
;; and be ignored.

(define-type (timespec)
  (timespec-time type: time?)
  (timespec-sign type: (memv '(+ -)))
  ;; types:
  ;; w - wall clock time (local time)
  ;; s - standard time without daylight savings adjustments
  ;; u, g, z - Universal time
  (timespec-type type: char?))

;;; TODO remove make-timespec
;;; It's a transient procedure while changing object system
(define (make-timespec time sign type)
  (timespec timespec-time: time
            timespec-sign: sign
            timespec-type: type))

(define (timespec-zero)
  (make-timespec (time) '+ #\w))

(define (timespec-add . specs)
  (unless (apply eqv? (map timespec-type specs))
    (warning (G_ "Adding timespecs of differing types")))

  (reduce (lambda (spec done)
            (cond
             ;; - -
             ;; + +
             [(eq? (timespec-sign done)
                   (timespec-sign spec))
              (modify done timespec-time
                      time+ (timespec-time spec))]
             ;; - +
             [(and (eq? '- (timespec-sign done))
                   (eq? '+ (timespec-sign spec)))
              (let ((time-a (timespec-time done))
                    (time-b (timespec-time spec)))
                (if (time< time-a time-b)
                    (make-timespec (time- time-b time-a)
                                   '- (timespec-type done))
                    (make-timespec (time- time-a time-b)
                                   '+ (timespec-type done))
                    ))]
             ;; + -
             [(and (eq? '+ (timespec-sign done))
                   (eq? '- (timespec-sign spec)))
              (let ((time-a (timespec-time done))
                    (time-b (timespec-time spec)))
                (if (time< time-a time-b)
                    (make-timespec (time- time-b time-a)
                                   '- (timespec-type done))
                    (make-timespec (time- time-a time-b)
                                   '+ (timespec-type done))
                    ))]))
          (timespec-zero)
          specs))


(define* (parse-time-spec
                 string optional: (suffixes '(#\s #\w #\u #\g #\z)))
  (let ((type string
          (cond [(string-rindex string (list->char-set suffixes))
                 => (lambda (idx)
                      (values (string-ref string idx)
                              (substring string 0 idx)))]
                [else (values #\w string)])))
    ;; Note that string->time allows a longer format than the input
    (cond [(string=? "-"  string)
           (make-timespec (time) '+ type)]
          [(string-prefix? "-" string)
           (make-timespec (string->time (string-drop string 1) "~H:~M:~S")
                          '- type)]
          [else
           (make-timespec (string->time string "~H:~M:~S" return-trailing: #t)
                          '+ type)])))

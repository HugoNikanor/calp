(define-module (vcomponent recurrence internal)
  #:use-module (util)
  #:use-module ((ice-9 optargs) #:select (define*-public))
  #:use-module (srfi srfi-88)
  #:export (make-recur-rule
            weekdays intervals))

;; Immutable, since I easily want to be able to generate the recurence set for
;; the same event multiple times.
(define-quick-record recur-rule
  (public: freq until count interval bysecond byminute byhour
           byday bymonthday byyearday byweekno bymonth bysetpos
           wkst))

(define (make-recur-rule interval wkst)
  ((record-constructor <recur-rule> '(interval wkst)) interval wkst))

;; TODO make this part of define-quick-record.
;; Only "hard" part would be to provide type hints for fields for
;; string conversions.
(define-public (format-recur-rule r)
  (define (a f)
    ((record-accessor <recur-rule> f) r))
  (with-output-to-string
    (lambda ()
      (format #t "#<recur-rule>~%")
      (for-each
       (lambda (field)
         (when (a field)
           (format #t "   ~8@a: ~a~%"
                   field
                   ((case field
                      ((until) (@ (srfi srfi-19 util) time->string))
                      (else identity))
                    (a field)))))
       (record-type-fields <recur-rule>)))))

(define*-public (print-recur-rule r #:optional (port (current-output-port)))
  (display (format-recur-rule r) port))

(define weekdays
  '(SU MO TU WE TH FR SA))

(define intervals
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))

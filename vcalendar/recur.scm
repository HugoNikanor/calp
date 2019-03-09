(define-module (vcalendar recur)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)        ; Records
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-26)           ; Cut
  #:use-module (srfi srfi-41)           ; Streams
  #:use-module (ice-9 curried-definitions)
  #:use-module (vcalendar)
  #:use-module (vcalendar datetime)
  #:use-module (util)

  #:export (recur-event))

;; (build-recur-rules "FREQ=HOURLY")
;; ;; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: #f>
;; (build-recur-rules "FREQ=HOURLY;COUNT=3")
;; ;; => #<<recur-rule> freq: HOURLY until: #f count: 3 interval: #f>
;; (build-recur-rules "FREQ=ERR;COUNT=3")
;; ;; => #<<recur-rule> freq: #f until: #f count: 3 interval: #f>
;; (build-recur-rules "FREQ=HOURLY;COUNT=err")
;; ;; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: #f>
;; (build-recur-rules "FREQ=HOURLY;COUNT=-1")
;; ;; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: #f>

(define-quick-record recur-rule
  freq until count interval bysecond byminute byhour wkst)

(define (build-recur-rules str)
  "Takes a RECUR value (string), and returuns a <recur-rule> object"
 (catch #t
  (lambda () (%build-recur-rules str))
  (lambda (err cont obj key val . rest)
    (let ((fmt (case err
                 ((unfulfilled-constraint)
                  "ERR ~a [~a] doesn't fulfill constraint of type [~a], ignoring~%")
                 ((invalid-value)
                  "ERR ~a [~a] for key [~a], ignoring.~%")
                 (else "~a ~a ~a"))))
      (format #t fmt err val key))
    (cont obj))))

;;; A special form of case only useful in build-recur-rules above.
;;; Each case is on the form (KEY val check-proc) where:
;;; `key` is what should be matched against, and what is used for the setter
;;; `val` is the value to bind to the loop object and
;;; `check` is something the object must conform to

(define-syntax-rule (throw-returnable symb args ...)
  (call/cc (lambda (cont) (throw symb cont args ...))))

(eval-when (expand)
 (define ((handle-case stx obj) key val proc)
   (with-syntax ((skey (datum->syntax
                        stx (symbol-downcase (syntax->datum key)))))
     #`((#,key)
        (let ((v #,val))
          (cond ((not v) (throw-returnable 'invalid-value #,obj (quote #,key) v))
                ((#,proc #,val) (set! (skey #,obj) v))
                (else (set! (skey #,obj)
                            (throw-returnable 'unfulfilled-constraint
                                              #,obj (quote #,key) v)))))))))

(define-syntax quick-case
  (lambda (stx)
   (syntax-case stx ()
     ((_ var-key obj (key val proc) ...)
      #`(case var-key
          #,@(map (handle-case stx #'obj)
                  #'(key ...)
                  #'(val ...)
                  #'(proc ...))
        (else obj))))))

(define weekdays
  '(SU MO TU WE TH FR SA))

(define intervals
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))

(define (string->number-list val delim)
  (map string->number (string-split val delim)))

(define (string->symbols val delim)
  (map string->symbol (string-split val delim)))

(define (%build-recur-rules str)
  (fold
   (lambda (lst obj)
     (let* (((key val) lst))
       (quick-case (string->symbol key) obj
                   (FREQ     (string->symbol val) (cut memv <> intervals))
                   (UNTIL    (parse-datetime val) identity)
                   (COUNT    (string->number val) (cut <= 0 <>))
                   (INTERVAL (string->number val) (cut <= 0 <>))
                   (BYSECOND (string->number-list val #\,) (cut every (cut <= 0 <> 60) <>))
                   (BYMINUTE (string->number-list val #\,) (cut every (cut <= 0 <> 59) <>))
                   (BYHOUR   (string->number-list val #\,) (cut every (cut <= 0 <> 23) <>))
                   ;; TODO implement these
                   ;; (BYDAY)
                   ;; (BYMONTHDAY)
                   ;; (BYYEARDAY)
                   ;; (BYWEEKNO)
                   ;; (BYMONTH)
                   ;; (BYSETPOS)
                   (WKST (string->symbol val) (cut memv <> weekdays))
                   )))

   ;; obj
   ((record-constructor <recur-rule> '(interval wkst)) 1 'MO)

   ;; ((key val) ...)
   (map (cut string-split <> #\=)
        (string-split str #\;))))

(define (generate-next event rule)
  (let ((ne (copy-vcomponent event)))   ; new event
    (case (freq rule)
      ((WEEKLY)
       (mod! (attr ne "DTSTART") (cut time-add <> 1 weeks))

       (set! (attr ne "DTEND")
             (add-duration (attr ne "DTSTART")
                           (attr ne "DURATION")))
       (values ne rule))

      ((DAILY)
       (mod! (attr ne "DTSTART") (cut time-add <> 1 days))

       (set! (attr ne "DTEND")
             (add-duration  (attr ne "DTSTART")
                            (attr ne "DURATION")))
       (values ne rule))

      (else (values '() rule)))))

(define-stream (recur-event-stream event rule-obj)
  (stream-cons event
               (let* ([next-event next-rule (generate-next event rule-obj)])
                 (if (null? next-event)
                     stream-null
                     (recur-event-stream next-event next-rule)))))

(define (recur-event event)
  (unless (attr event "DURATION")
    (set! (attr event "DURATION")
          (time-difference
           (attr event "DTEND")
           (attr event "DTSTART"))))
  (recur-event-stream event (build-recur-rules (attr event "RRULE"))))

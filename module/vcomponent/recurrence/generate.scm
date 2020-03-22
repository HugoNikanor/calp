(define-module (vcomponent recurrence generate)
  #:use-module ((srfi srfi-1) :select (find))
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (srfi srfi-26)           ; Cut
  #:use-module (srfi srfi-41)           ; Streams
  #:use-module (ice-9 match)

  #:use-module (util)
  #:use-module (vcomponent base)
  #:use-module (vcomponent recurrence internal)
  #:use-module (vcomponent recurrence parse)

  #:export (generate-recurrence-set)
  )

;;; TODO implement
;;; EXDATE and RDATE

;;; EXDATE (3.8.5.1)
;;; comma sepparated list of dates or datetimes.
;;; Can have TZID parameter
;;; Specifies list of dates that the event should not happen on, even
;;; if the RRULE say so.
;;; Can have VALUE field specifiying "DATE-TIME" or "DATE".

;;; RDATE (3.8.5.2)
;;; Comma sepparated list of dates the event should happen on.
;;; Can have TZID parameter.
;;; Can have VALUE parameter, specyfying "DATE-TIME", "DATE" or "PREIOD".
;;; PERIOD (see 3.3.9)

(define (seconds-in freq)
  (case freq
    ((SECONDLY) 1)
    ((MINUTELY) 60)
    ((HOURLY) (* 60 60))
    ((DAILY) (* 60 60 24))
    ((WEEKLY) (* 60 60 24 7))))

;; Event x Rule → Event
;; TODO My current naïve aproach to simple adding a constant time to an event
;; breaks with time-zones. betwen 12:00 two adjacent days might NOT be 24h.
;; Specifically, 23h or 25h when going between summer and "normal" time.

(define (next-event ev r)
  (let ((e (copy-vcomponent ev)))
    (let-env ((TZ (and=> (prop (attr* e 'DTSTART) 'TZID) car)))

             (let ((d (attr e 'DTSTART))
                   (i (interval r)))

               (set! (attr e 'DTSTART)
                 ((if (date? d)
                    identity
                    (lambda (date)
                      (datetime
                        date: date
                        time: (time+ (get-time d)
                                     (case (freq r)
                                       ((SECONDLY) (time second: i))
                                       ((MINUTELY) (time minute: i))
                                       ((HOURLY)   (time hour:   i))
                                       (else (time)))))))

                  (date+ (as-date d)
                         (case (freq r)
                           ((DAILY)    (date day:    i))
                           ((WEEKLY)   (date day: (* i 7)))
                           ((MONTHLY)  (date month:  i))
                           ((YEARLY)   (date year:   i))
                           (else (date))))))

               #;
               (set! (zone-offset d)
                 (zone-offset (time-utc->date (date->time-utc d))))


               (let ((start (attr e 'DTSTART))
                     (end (attr e 'DTEND))
                     (change (attr e 'X-HNH-DURATION)))
                 (when end
                   (set! (attr e 'DTEND)
                     (if (date? start)
                         (date+ start change)
                         (datetime+ start (datetime time: change))))))))

    e))

;; BYDAY and the like depend on the freq?
;; Line 7100
;; Table @@ 2430
;; 
;; Event x Rule → Bool (continue?)
;; Alternative, monadic solution using <optional>.
;; @example
;; (optional->bool
;;  (do (<$> (cut time<=? (attr last 'DTSTART)) (until r))
;;      (<$> (negate zero?) (count r))
;;    (just #t)))
;; @end example
(define-stream (recur-event-stream event rule-obj)
  (stream-unfold

   ;; Event x Rule → Event
   (match-lambda
     ((e _)
      (let ((expected-start (attr e 'DTSTART)))
        ;; If we have alternatives, check them
        (or (and=> (attr e 'X-HNH-ALTERNATIVES)
                   (lambda (alternatives)
                     ;; A recurrence id matching the expected time means that
                     ;; we have an actuall alternative/exception, use that
                     ;; instead of the regular event.
                     ;; RFC 5545 3.8.4.4
                     (find (lambda (alt) ((if (datetime? expected-start)
                                         datetime= date=)
                                     expected-start (attr alt 'RECURRENCE-ID)))
                           alternatives)))
            ;; If we did't have an exception just return the regular event.
            e))))

   ;; Event x Rule → Bool (continue?)
   (match-lambda
     ((e r)
      (or (and (not (until r)) (not (count r)))                   ; Never ending
          (and=> (count r) (negate zero?))                        ; COUNT
          (and=> (until r) (lambda (dt) ((if (date? dt) date<= date/-time<=) ; UNTIL
                                    (attr e 'DTSTART) dt))))))

   ;; Event x Rule → next (Event, Rule)
   (match-lambda
     ((e r)
      (list (next-event e r)
            (if (count r)
                ;; Note that this doesn't modify, since r is immutable.
                (mod! (count r) 1-)
                r ))))

   ;; Seed
   (list event rule-obj)))


(define (generate-recurrence-set event)
  ;; TODO DURATION might be used for something else, check applicable types
  ;; TODO Far from all events have DTEND
  ;;      VTIMEZONE's always lack it.
  (catch #t
    (lambda ()
     (if (not (attr event 'RRULE))
         (stream event)
         (begin
           (set! (attr event 'X-HNH-DURATION)
             (cond [(attr event 'DURATION) => identity]
                   [(attr event 'DTEND)
                    => (lambda (end)
                         ;; The value type of dtstart and dtend must be the same
                         ;; according to RFC 5545 3.8.2.2 (Date-Time End).
                         (if (date? end)
                             (date-difference end (attr event 'DTSTART))
                             (time second: (datetime-difference end (attr event 'DTSTART)))))]))
           (if (attr event "RRULE")
               (recur-event-stream event (parse-recurrence-rule
                                          (attr event "RRULE")
                                          (if (eq? 'DATE (and=> (prop (attr* event 'DTSTART) 'VALUE) car))
                                              parse-date parse-datetime)))
               ;; TODO some events STANDARD and DAYLIGT doesn't have RRULE's, but rather
               ;; just mention the current part. Handle this
               stream-null))))
    (lambda (err . args)
      (format (current-error-port)
              "\x1b[0;31mError\x1b[m while parsing recurrence rule (ignoring and continuing)~%~a ~s~%~a~%~%"
              err args
              (attr event 'X-HNH-FILENAME))
      (stream ; event
       )
      )))

(define-module (vcalendar recurrence generate)
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-19 setters)
  #:use-module (srfi srfi-26)           ; Cut
  #:use-module (srfi srfi-41)           ; Streams
  #:use-module (ice-9 match)

  #:use-module (util)
  #:use-module (vcalendar)
  #:use-module (vcalendar recurrence internal)
  #:use-module (vcalendar recurrence parse)

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
(define (next-event ev r)
  (let ((e (copy-vcomponent ev)))
    (cond
     ((memv (freq r) '(SECONDLY MINUTELY HOURLY DAILY WEEKLY))
      (mod! (attr e 'DTSTART)
            ;; Previously I had the mutating version of
            ;; @var{add-duration(!)} here. However since
            ;; @var{copy-vcomponent} doesn't do a deep copy that
            ;; modified the attribute for all items in the set,
            ;; breaking everything.
            (cut add-duration <>
                 (make-duration
                  (* (interval r)        ; INTERVAL
                     (seconds-in (freq r)))))))

     ((memv (freq r) '(MONTHLY YEARLY))
      (let ((sdate (time-utc->date (attr e 'DTSTART))))
        (case (freq r)
          ((MONTHLY) (mod! (month sdate) (cut + <> (interval r))))
          ((YEARLY)  (mod! (year sdate)  (cut + <> (interval r)))))
        (set! (attr e 'DTSTART)
              (date->time-utc sdate))))

     ;; TODO
     ;; All the BY... fields
     )

    (when (attr e 'DTEND)
     (set! (attr e 'DTEND)
           (add-duration (attr e 'DTSTART) (attr e 'DURATION))))

    ;; Return
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
   car

   ;; Event x Rule → Bool (continue?)
   (match-lambda
     ((e r)
      (or (and (not (until r)) (not (count r)))                   ; Never ending
          (and=> (count r) (negate zero?))                        ; COUNT
          (and=> (until r) (cut time<=? (attr e 'DTSTART) <>))))) ; UNTIL

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
  (if (not (attr event 'RRULE))
      (stream event)
      (begin
        (when (and (attr event 'DTEND)
                   (not (attr event 'DURATION)))
          (set! (attr event "DURATION")
                (time-difference
                 (attr event "DTEND")
                 (attr event "DTSTART"))))
        (if (attr event "RRULE")
            (recur-event-stream event (parse-recurrence-rule (attr event "RRULE")))
            ;; TODO some events STANDARD and DAYLIGT doesn't have RRULE's, but rather
            ;; just mention the current part. Handle this
            stream-null))))

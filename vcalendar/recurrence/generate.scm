(define-module (vcalendar recurrence generate)
  ;; #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-9 gnu)        ; Records
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)

  #:use-module (srfi srfi-26)           ; Cut
  #:use-module (srfi srfi-41)           ; Streams
  #:use-module (ice-9 control)          ; ?
  #:use-module (ice-9 match)
  #:use-module (vcalendar)
  #:use-module (vcalendar datetime)
  #:use-module (util)

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

(define (seconds-in interval)
  (case interval
    ((SECONDLY) 1)
    ((MINUTELY) 60)
    ((HOURLY) (* 60 60))
    ((DAILY) (* 60 60 24))
    ((WEEKLY) (* 60 60 24 7))))

(define-stream (recur-event-stream event rule-obj)
  (stream-unfold
   ;; Rule → event
   (match-lambda
     ((last r)
      (let ((e (copy-vcomponent last)))   ; new event
        ;; TODO
        ;; Update DTEND as (add-duration DTSTART DURATINO)
        (cond

         ;; BYDAY and the like depend on the freq?
         ;; Line 7100
         ;; Table @ 2430

         ((memv (freq r) '(SECONDLY MINUTELY HOURLY DAILY WEEKLY))
          (mod! (attr e "DTSTART")
                (cut add-duration! <>
                     (make-duration
                      ;; INTERVAL
                      (* (interval r)
                         (seconds-in (freq r)))))))

         ((memv (freq r) '(MONTHLY YEARLY))
          ;; Hur fasen beräkrnar man det här!!!!
          #f
          )

         (else #f))
        e)))

   ;; Rule → Bool (#t if continue, #f if stop)
   (match-lambda
     ((last r)

      ;; (optional->bool
      ;;  (do (<$> (cut time<=? (attr last 'DTSTART)) (until r))
      ;;      (<$> (negate zero?) (count r))
      ;;    (just #t)))

      (or (and (not (until r)) (not (count r)))
          (and=> (until r) (cut time<=? (attr last 'DTSTART) <>)) ; UNTIL
          (and=> (count r) (negate zero?)))                      ; COUNT

      )
     )

   ;; Rule → (next) Rule
   (match-lambda
     ((last r)
      ;; Note that this doesn't modify, since r is immutable.
      (list last
            (if (count r)
                (mod! (count r) 1-)
                r))))
   (list event rule-obj)))


(define (generate-recurrence-set event)
  (unless (attr event "DURATION")
    (set! (attr event "DURATION")
          (time-difference
           (attr event "DTEND")
           (attr event "DTSTART"))))
  (recur-event-stream event (build-recur-rules (attr event "RRULE"))))

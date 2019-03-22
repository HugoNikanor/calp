(define-module (vcalendar recurrence generate)
  ;; #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-9 gnu)        ; Records
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)

  #:use-module (srfi srfi-26)           ; Cut
  #:use-module (srfi srfi-41)           ; Streams
  ;; #:use-module (ice-9 control)          ; call-with-escape-continuation
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

(define (seconds-in freq)
  (case freq
    ((SECONDLY) 1)
    ((MINUTELY) 60)
    ((HOURLY) (* 60 60))
    ((DAILY) (* 60 60 24))
    ((WEEKLY) (* 60 60 24 7))))


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
     ((last r)
      (let ((e (copy-vcomponent last)))   ; new event
        (cond

         ((memv (freq r) '(SECONDLY MINUTELY HOURLY DAILY WEEKLY))
          (mod! (attr e 'DTSTART)       ; MUTATE
                (cut add-duration! <>
                     (make-duration
                      (* (interval r)   ; INTERVAL
                         (seconds-in (freq r)))))))

         ((memv (freq r) '(MONTHLY YEARLY))
          #f ; Hur fasen beräkrnar man det här!!!!
          ))

        ;; TODO this is just here for testing
        (mod! (attr e 'NEW_ATTR) not)   ; MUTATE
        ;; This segfaults...
        ;; (set! (attr e 'N) #t)           ; MUTATE
        ((@ (vcalendar output) print-vcomponent) e)
        (set! (attr e 'D) #t)

        (set! (attr e 'DTEND)           ; MUTATE
              (add-duration
               (attr e 'DTSTART)
               (attr e 'DURATION)))
        e)))

   ;; Event x Rule → Bool (continue?)
   (match-lambda
     ((e r)

      (or (and (not (until r)) (not (count r)))                   ; Never ending
          (and=> (count r) (negate zero?))                        ; COUNT
          (and=> (until r) (cut time<=? (attr e 'DTSTART) <>))))) ; UNTIL

   ;; _ x Rule → (_, (next) Rule)
   (match-lambda
     ((e r)
      (list
       e (if (count r)
             ;; Note that this doesn't modify, since r is immutable.
             (mod! (count r) 1-)
             r))))

   ;; Seed
   (list event rule-obj)))


(define (generate-recurrence-set event)
  (unless (attr event "DURATION")
    (set! (attr event "DURATION")       ; MUTATE
          (time-difference
           (attr event "DTEND")
           (attr event "DTSTART"))))
  (recur-event-stream event (parse-recurrence-rule (attr event "RRULE"))))

  ;; How doee stream-unfold even work?
  ;; What element is used as the next seed?
;;; stream-fold:
;; (stream-let recur ((base base))
;;     (if (pred? base)
;;         (stream-cons (mapper base) (recur (generator base)))
;;         stream-null))

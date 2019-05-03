(define-module (vcomponent timezone)
  :use-module (vcomponent base)
  :use-module ((srfi srfi-1) :select (find))
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-19 util)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (util)
  :use-module ((vcomponent recurrence generate) :select (generate-recurrence-set))
  :use-module ((vcomponent datetime) :select (ev-time<?))
  )

;;@begin exampe
;; <VTIMEZONE> :: "#<vcomponent 558c5da80fc0>"
;;                  TZID: Europe/Stockholm
;;        X-LIC-LOCATION: Europe/Stockholm
;; : <DAYLIGHT> :: "#<vcomponent 558c5e11e7c0>"
;; :                RRULE: FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
;; :              DTSTART: 19700329T020000
;; :               TZNAME: CEST
;; :           TZOFFSETTO: +0200
;; :         TZOFFSETFROM: +0100
;; : <STANDARD> :: "#<vcomponent 558c5e11e7e0>"
;; :                RRULE: FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
;; :              DTSTART: 19701025T030000
;; :               TZNAME: CET
;; :           TZOFFSETTO: +0100
;; :         TZOFFSETFROM: +0200
;; @end example

;; Given a tz stream of length 2, takes the time difference between the DTSTART
;; of those two. And creates a new VTIMEZONE with that end time.
;; TODO set remaining properties, and type of the newly created component.
(define (extrapolate-tz-stream strm)
  (let ((nevent (copy-vcomponent (stream-ref strm 1))))
    (mod! (attr nevent 'DTSTART)
          = (add-duration (time-difference
                           (attr (stream-ref strm 1) 'DTSTART)
                           (attr (stream-ref strm 0) 'DTSTART))))
    (stream-append strm (stream nevent))))

;; The RFC requires that at least one DAYLIGHT or STANDARD component is present.
;; Any number of both can be present. This should handle all these cases well,
;; as long as noone has multiple overlapping timezones, which depend on some
;; further condition. That feels like something that should be impossible, but
;; this is (human) time we are talking about.
(define-public (make-tz-set tz)
  (let ((strm (interleave-streams
               ev-time<?
               ;; { DAYLIGHT, STANDARD }
               (map generate-recurrence-set (children tz)))))

    (cond [(stream-null? strm) stream-null]

          [(stream-null? (stream-drop 2 strm))
           (let ((strm (extrapolate-tz-stream strm)))
             (stream-zip strm (stream-cdr strm)))]

          [else (stream-zip strm (stream-cdr strm))])))

(define (parse-offset str)
  (let* (((pm h1 h0 m1 m0) (string->list str)))
    ((primitive-eval (symbol pm))
     (+ (* 60 (string->number (list->string (list m1 m0))))
        (* 60 60 (string->number (list->string (list h1 h0))))))))

;; Finds the VTIMEZONE with id @var{tzid} in calendar.
;; Crashes on error.
(define (find-tz cal tzid)
  (let ((ret (find (lambda (tz) (string=? tzid (attr tz 'TZID)))
                   (children cal 'VTIMEZONE))))
    ret))

;; Takes a VEVENT.
;; Assumes that DTSTART has a TZID property, and that that TZID is available as
;; a direct child of the parent of @var{ev}.
(define-public (get-tz-offset ev)
  (let ((ret (stream-find
              (lambda (z)
                (let* (((start end) (map (extract 'DTSTART) z)))
                  (and (time<=? start (attr ev 'DTSTART))
                       (time<? (attr ev 'DTSTART) end))))
              (attr (find-tz (parent ev)
                             (car (prop (attr* ev 'DTSTART) 'TZID)))
                    'X-HNH-TZSET))))
    (if (not ret)
        0 (parse-offset (attr (car ret) 'TZOFFSETTO)))))


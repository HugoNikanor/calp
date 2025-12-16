;;; TODO document this module
(define-module (vcomponent datetime)
  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-41) :select (stream-filter))
  :use-module ((srfi srfi-41 util) :select (get-stream-interval))
  :use-module (vcomponent)
  :use-module (vcomponent create)
  :use-module (vcomponent type duration)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (datetime zic)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util type)
  :use-module (hnh util exceptions)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 match)

  :export (
           instance-overlaps?
           overlapping?
           instance-zero-length?

           instance-length
           instance-length/clamped
           instance-length/day

           long-instance?

           events-between

           ))


;;; NOTE all these procedures assume well well formed vevent instances.
;;; This means that DTSTART MUST be present, and that DTEND and
;;; DURATION isn't explicitly checked for type, but instead assumed to
;;; match DTSTART (and so on).

(define (instance-overlaps? event begin end)
  "Check if the event overlaps the timespan."
  (typecheck event vevent?)
  (timespan-overlaps? (as-datetime (prop1 event 'DTSTART))
                      (instance-end event)
                      (as-datetime begin) (as-datetime end)))

;;; Check if two instances of events overlap
(define (overlapping? event-a event-b)
  (typecheck event-a vevent?)
  (typecheck event-b vevent?)
  (timespan-overlaps? (as-datetime (prop1 event-a 'DTSTART))
                      (instance-end event-a)
                      (as-datetime (prop1 event-b 'DTSTART))
                      (instance-end event-b)))

(define (instance-zero-length? ev)
  (typecheck ev vevent?)
  (define start (prop1 ev 'DTSTART))
  (or (and=> (prop1 ev 'DURATION)
             (lambda (dur) (datetime= (datetime) ((unval duration->datetime 1) dur))))
      (and (datetime? start)
           (or (and (not (prop1 ev 'DTEND))
                    (not (prop1 ev 'DURATION)))
               (and=> (prop1 ev 'DTEND)
                      (lambda (end) (datetime= start end)))))
      (and (date? start)
           (and=> (prop1 ev 'DTEND)
                  (lambda (end) (date= start end))))))

(define (instance-end e)
  (cond ((prop1 e 'DURATION)
         => (lambda (d)
              (datetime+ (as-datetime (prop1 e 'DTSTART))
                         ((unval duration->datetime 1) d))))
        ((prop1 e 'DTEND) => as-datetime)
        (else
         (datetime+ (as-datetime (prop1 e 'DTSTART))
                    (instance-length e)))))

(define (instance-length e)
  (let ((s (prop1 e 'DTSTART)))
   (cond ((prop1 e 'DURATION) => (unval duration->datetime 1))
         ((prop1 e 'DTEND)
          => (lambda (d)
               (datetime-difference (as-datetime d)
                                    (as-datetime s))))
         (else
          (cond ((date? s)    (datetime day: 1))
                ((datetime? s) (datetime))
                (else (scm-error 'misc-error "instance-length"
                                 "Non date or datetime object found in DTSTART: ~s"
                                 (list s) #f)))))))

;;
;; |-----|      extent of event
;;     |-----|  time we are interested in,
;;              defined through @var{start-date} and @var{end-date}
;;     |X|      part of event within that time (X)
;; 
;; Returns the length of the interval `X`, as a datetime object
(define (instance-length/clamped start-date end-date e)
  (typecheck start-date date?)
  (typecheck end-date   date?)
  (typecheck e vevent?)

  (datetime-difference
   (datetime-min (instance-end e)
                 (datetime date: (date+ end-date (date day: 1))))
   (datetime-max (as-datetime (prop1 e 'DTSTART))
                 (datetime date: start-date))))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
;; currently the second argument is a date, but should possibly be changed
;; to a datetime to allow for more explicit TZ handling?
(define (instance-length/day date e)
  (typecheck date date?)
  (typecheck e vevent?)

  (if (not (prop1 e 'DTEND))
      (if (date? (prop1 e 'DTSTART))
          (time hour: 24)
          (time))
      (let ((start (prop1 e 'DTSTART))
            (end (prop1 e 'DTEND)))
        (cond [(date= date (as-date start) (as-date end))
               (time- (as-time end) (as-time start))]
              ;; Starts today, end in future day
              [(date= date (as-date start))
               (time- (time hour: 24) (as-time start))]
              ;; Ends today, start earlier day
              [(date= date (as-date end))
               (as-time end)]
              ;; start earlier date, end later date
              [else (time hour: 24)]))))


;; 22:00 - 03:00
;; 2h för dag 1
;; 3h för dag 2

;; An event is considered long if it's DTSTART (and thereby DTEND) lacks a time component,
;; or if the total length of the event is greater than 24h.
;; For practical purposes, an event being long means that it shouldn't be rendered as a part
;; of a regular day.
(define (long-instance? ev)
  (or (date? (prop1 ev 'DTSTART))
      (datetime<= (datetime day: 1)
                  (instance-length ev))))

;; date, date, [sorted-stream events] → [sorted-stream events]
;; DEPRECATED this is only useful when all events are in a single
;; stream, which they haven't been since the introduction of data
;; stores. See
;; (@ (vcomponent type recurrence) expand-and-interleave-recurrences)
;; instead
(define (events-between start-date end-date events)
  (define (overlaps e)
    (timespan-overlaps? start-date (date+ end-date (date day: 1))
                        ;; TODO DURATION
                        (prop1 e 'DTSTART) (or (prop1 e 'DTEND)
                                               (prop1 e 'DTSTART))))

  (stream-filter
   overlaps
   (get-stream-interval
    overlaps
    (lambda (e) (not (date< end-date (as-date (prop1 e 'DTSTART)))))
    events)))

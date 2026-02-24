;;; TODO document this module
(define-module (vcomponent datetime)
  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-41) :select (stream-filter stream-null? stream-take-while))
  :use-module ((srfi srfi-41 util) :select (get-stream-interval stream-of))
  :use-module (vcomponent)
  :use-module (vcomponent media-type)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent create)
  :use-module (vcomponent type duration)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (datetime zoneinfo)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util type)
  :use-module (hnh util exceptions)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 match)
  :use-module ((rnrs base) :select (assert) :version (6))

  :export (
           event-overlaps?
           instance-overlaps?
           instances-overlap?

           instance-start-datetime
           instance-length
           instance-length/clamped

           ))


;;; NOTE all these procedures assume well well formed vevent instances.
;;; This means that DTSTART MUST be present, and that DTEND and
;;; DURATION isn't explicitly checked for type, but instead assumed to
;;; match DTSTART (and so on).

;;; NOTE many procedures here references instances.
;;; That means a specific VEVENT insntance, which MAY be part of a
;;; recurrence set.



;;; Returns the start datetime of an instance, as a zoned datetime object.
;;; If the start already was in a timezone, than that timezone is kept, otherwise
;;; the reference-zone is used.
(define (instance-start-datetime reference-zone instance)
  (typecheck reference-zone string?)
  (typecheck instance vevent?)

  (ensure-zoned-datetime reference-zone (prop1 instance 'DTSTART)))


;; Returns the length of the event, as an unzoned datetime object.
;; This IS timezone aware, meaning that start and end can be in any timezones
;; (standard mentions flights, which preferably have departure and
;; arrival time in the time of the Airport, while the duration becomes
;; the total flight duration. For example, consider the flight:
;; Departure: 10:45 Stockholm
;; Arrival: 13:35 New York
;; (datetime-difference/zoneinfo (tz #2026-01-16T13:35 "America/New_York")
;;                               (tz #2026-01-16T10:45 "Europe/Stockholm"))
;; ⇒ #0000-00-00T08:50:00
;; (and NOT 2:50 as an zone-unaware thing would work)).
;;
;; TODO Exact value when a timezone changes (usually due to DST changeover) is currently UNDEFINED.
(define (instance-length e)
  (let ((s (prop1 e 'DTSTART)))
    (cond ((prop1 e 'DURATION) => identity)
          ((prop1 e 'DTEND)
           => (lambda (end)
                (cond ((date? s)             (date-difference end s))
                      ((unzoned-datetime? s) (datetime-difference end s))
                      ((zoned-datetime? s)   (datetime-difference/zoneinfo end s))
                      (else (scm-error 'misc-error "instance-length"
                                       "Start of event of unknown type: ~s"
                                       (list s) #f)))))
          ((date? s)     (duration day: 1))
          ((datetime? s) (duration))
          (else (scm-error 'misc-error "instance-length"
                           "Non date or datetime object found in DTSTART: ~s"
                           (list s) #f)))))



;; Does any instance of the calendar event overlaps the timespan?
(define (event-overlaps? reference-zone event start end)
  (typecheck event vcalendar?)
  (assert (every (lambda (x) (or (vevent? x)
                            (vtimezone? x)))
                 (vcomponent-children event)))
  (typecheck start zoned-datetime?)
  (typecheck end zoned-datetime?)

  (define utc-start ((unval zone->utc) start))
  (define utc-end   ((unval zone->utc) end))

  (cond ((recurring? event)
         (not
          (->> (generate-recurrence-set event)
               (stream-take-while
                (lambda (instance) (datetime</zoneinfo
                               (instance-start-datetime reference-zone instance)
                               utc-end)))
               (stream-filter
                (lambda (instance) (instance-overlaps? reference-zone instance utc-start utc-end)))
               stream-null?)))

        (else ; non-recurring
         (instance-overlaps? reference-zone
                             (find (lambda (x) (eq? 'VEVENT (type x)))
                                   (vcomponent-children event))
                             start end))))

(define (instance-overlaps? reference-zone event start end)
  "Check if the event overlaps the timespan."
  (typecheck event vevent?)

  (typecheck start zoned-datetime?)
  (typecheck end   zoned-datetime?)

  (define st (instance-start-datetime reference-zone event))
  (define et (datetime+ st (instance-length event)))

  (timespan-overlaps? ((unval zone->utc) st)
                      ((unval zone->utc) et)
                      ((unval zone->utc) start)
                      ((unval zone->utc) end)))


;;; Check if two instances of events overlap
;;; Reference zone is used to resolve dates and datetimes in "local" time.
(define (instances-overlap? reference-zone event-a event-b)
  (typecheck reference-zone string?)
  (typecheck event-a vevent?)
  (typecheck event-b vevent?)

  (define start-a ((unval zone->utc) (instance-start-datetime reference-zone event-a)))
  (define start-b ((unval zone->utc) (instance-start-datetime reference-zone event-b)))

  ;; NOTE this inherits the timezone considerations from instance-length
  (define end-a (datetime+ start-a (instance-length event-a)))
  (define end-b (datetime+ start-b (instance-length event-b)))

  (timespan-overlaps? start-a end-a
                      start-b end-b))

;;
;; |-----|      extent of event
;;     |-----|  time we are interested in,
;;              defined through @var{start-date} and @var{end-date}
;;     |X|      part of event within that time (X)
;; 
;; Returns the length of the interval `X`, as a datetime object
(define (instance-length/clamped start-dt end-dt reference-tz e)
  (typecheck start-dt zoned-datetime?)
  (typecheck end-dt   zoned-datetime?)
  (typecheck e vevent?)

  ;; TODO rewrite this into a timespan-overlap procedure, which takes
  ;; two timespans and returns the overlap between the two
  ;; This MUST be suitable to send to datetime-difference to get the length of the timespan.

  ;; TODO this should be changed to work on zones

  (define st (instance-start-datetime reference-tz e))

  (define st-utc ((unval zone->utc) st))
  (define et-utc ((unval zone->utc) (datetime+ st (instance-length e))))
  (define start-dt-utc ((unval zone->utc) end-dt))
  (define end-dt-utc   ((unval zone->utc) start-dt))

  (if (timespan-overlaps? start-dt-utc end-dt-utc
                          st-utc et-utc)
      (datetime-difference
       (datetime-min start-dt-utc et-utc)
       (datetime-max end-dt-utc   st-utc))
      ;; TODO document the non-overlapping case
      (duration)))


(define-module (vcomponent type recurrence generate)
  :use-module (vcomponent)
  :use-module (vcomponent datetime)
  :use-module (vcomponent type recurrence internal)
  :use-module (vcomponent type period)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util exceptions)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util destructure)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-197)
  :use-module (ice-9 curried-definitions)
  :export (find-base-instance
           generate-recurrence-set
           expand-and-interleave-recurrences
           ))



;;; TODO move this to the "Commentary" section
;;; This is primarily for "dumb" data stores (e.g. file and vdir).
;;; The default assumption for these procedures are that they work on a single logical event (e.g. all vevent objects share an UID), wrapped in a vcalendar.
;;; Behaviour if a "complete" vcalendar with multiple distinct logical events (e.g. they have different UID's is *UNDEFINED*).


;; Find the base instance of a recurring event.
;; If a component consists an RRULE or RDATE property
;; then that component is choosen. Otherwise, the entry with the earliest DTSTART is selected.
;; Returns 2 values:
;; - the base instance of the event
;; - a list of all remaining entries, unordered
(define (find-base-instance event)
  (typecheck event vcalendar?)
  (define event-components
    (filter vevent? (vcomponent-children event)))
  (define focus
    (or (find (lambda (component)
                (or (prop1 component 'RRULE)
                    (prop1 component 'RDATE)))
              event-components)
        (find-extreme event-components datetime< instance-start-datetime)))

  (values focus (delq focus event-components)))




;; This table is copied "directly" from RFC5545, with just some minor
;; text masaging into making it valid scheme code.
;; Order of rows is IMPORTANT, since it is used to determine the order
;; in which the rules are applied. It goes from top to bottom.
;; Column order is insignificant, but must be kept in sync with
;; column-indices.
(define rrule-table
  (list->array ; Needed since (datetime) highjacked the #2 reader macro
   2
   '((_          || SECONDLY MINUTELY HOURLY  DAILY   WEEKLY MONTHLY YEARLY)
     ;; ---------||----------------------------------------------------------
     (BYMONTH    || Limit    Limit    Limit   Limit   Limit  Limit   Expand)
     (BYWEEKNO   || N/A      N/A      N/A     N/A     N/A    N/A     Expand)
     (BYYEARDAY  || Limit    Limit    Limit   N/A     N/A    N/A     Expand)
     (BYMONTHDAY || Limit    Limit    Limit   Limit   N/A    Expand  Expand)
     (BYDAY      || Limit    Limit    Limit   Limit   Expand Note-1  Note-2)
     (BYHOUR     || Limit    Limit    Limit   Expand  Expand Expand  Expand)
     (BYMINUTE   || Limit    Limit    Expand  Expand  Expand Expand  Expand)
     (BYSECOND   || Limit    Expand   Expand  Expand  Expand Expand  Expand)
     (BYSETPOS   || Limit    Limit    Limit   Limit   Limit  Limit   Limit))))

(define column-indices
 '((SECONDLY . 2) (MINUTELY . 3) (HOURLY . 4) (DAILY . 5)
   (WEEKLY . 6) (MONTHLY . 7) (YEARLY . 8)))

(define rrule-accessors
  (list
   (cons 'BYMONTH    bymonth)
   (cons 'BYWEEKNO   byweekno)
   (cons 'BYYEARDAY  byyearday)
   (cons 'BYMONTHDAY bymonthday)
   (cons 'BYDAY      byday)
   (cons 'BYHOUR     byhour)
   (cons 'BYMINUTE   byminute)
   (cons 'BYSECOND   bysecond)
   (cons 'BYSETPOS   bysetpos)))



;; Returns the first instance of the given week-day after @var{d}.
;; @example
;; (find-first-week-day mon #2020-04-01)
;; => #2020-04-06
;; (find-first-week-day mon #2020-04-10)
;; => #2020-04-13
;; (find-first-week-day mon #2020-04-30)
;; => #2020-05-04
;; @end example
(define (find-first-week-day wday d)
  (let* ((start-day (week-day d))
         (diff (- wday start-day)))
    (date+ d (duration day: (modulo diff 7)))))

;; returns instances of the given week-day in month between
;; month-date and end of month.
;; @example
;; (all-wday-in-month mon #2020-06-01)
;; => (#2020-06-01 #2020-06-08 #2020-06-15 #2020-06-22 #2020-06-29)
;; (all-wday-in-month mon #2020-06-10)
;; => (#2020-06-15 #2020-06-22 #2020-06-29)
;; @end example
;; week-day, date → (list date)
;; TODO remane procdure, and clarify documentation
(define (all-wday-in-month wday month-date)
  (date-range (find-first-week-day wday month-date)
              (end-of-month month-date)
              7))


(define (all-wday-in-year wday year-date)
  (date-range (find-first-week-day wday year-date)
              (end-of-year year-date)
              7))





(define ((limiter-positive-int dt-accessor) rrule-accessor rrule dt-list)
  (filter (lambda (dt) (memv (dt-accessor dt)
                     (or (rrule-accessor rrule) '())))
          dt-list))

(define ((limiter-int dt-accessor dt-max-value) rrule-accessor rrule dt-list)
  (filter (lambda (dt)
            (find (lambda (x)
                    (= x (if (positive? x)
                             (dt-accessor dt)
                             (- (dt-max-value dt)
                                (dt-accessor dt)))))
                  (or (rrule-accessor rrule) '())))
          dt-list))

(define (run-bysetpos _ rrule dt-list)
  (map (lambda (pos)
         (let ((len (length dt-list)))
           (list-ref dt-list
                     (if (positive? pos)
                         (- pos 1)
                         (- len (- pos))))))
       (bysetpos rrule)))

;; > Recurrence rules may generate recurrence instances with an invalid
;; > date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
;; > on a day where the local time is moved forward by an hour at 1:00
;; > AM).  Such recurrence instances MUST be ignored and MUST NOT be
;; > counted as part of the recurrence set.
;; To ensure we only have valid date(times), we add 0 to the date.
;; This causes the date to be re-normalized to a valid date,
;; effectively checking if we have a "real" date.

(define ((expander-int rule-applier) rrule-accessor rrule dt-list)
  (filter (lambda (dt) (datetime= dt (datetime+ dt (duration))))
          (append-map (lambda (dt)
                        (map (lambda (x) (rule-applier x dt))
                             (rrule-accessor rrule)))
                      dt-list)))

;;; TODO
;; > The WKST rule part specifies the day on which the workweek starts.
;; > Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is
;; > significant when a WEEKLY "RRULE" has an interval greater than 1,
;; > and a BYDAY rule part is specified.  This is also significant when
;; > in a YEARLY "RRULE" when a BYWEEKNO rule part is specified.  The
;; > default value is MO.

;; day-enumerator
;; rrule
;; dt-list
(define ((byday-expander day-enumerator start-of-interval) _ rrule dt-list)
  (append-map
   (lambda (dt)
     (append-map
      (lambda (day-spec)
        (map (lambda (d) (datetime-date dt d))
             (cond ((car day-spec)
                    => (lambda (c)
                         (list
                          (list-ref ((if (positive? c) identity reverse)
                                     (day-enumerator (cdr day-spec)
                                                     (start-of-interval
                                                      (datetime-date dt))))
                                    (1- (abs c))))))
                   (else
                    (day-enumerator (cdr day-spec)
                                    (start-of-interval
                                     (datetime-date dt)))))))
      (byday rrule)))
   dt-list))

;; (define ((byday-expander/year) rrule dt-list)
;;   (append-map
;;    (lambda (dt)
;;      ;; TODO this can easily create duplicates, for example if both FR and 1FR are noted.
;;      ;; This is however a problem for all types, see if RFC specifies something about it
;;      (append-map
;;       (lambda (day-spec)
;;         (cond ((car day-spec)
;;                => (lambda (c)
;;                     (list
;;                      (if (positive? c)
;;                          (list-ref (all-wday-in-year dt)
;;                                    (1- c))
;;                          (list-ref (reverse (all-wday-in-year dt))
;;                                    (1- c))))))
;;               (else
;;                (all-wday-in-year (cdr day-spec)
;;                                   dt))))
;;       (byday rrule)))
;;    dt-list)
;;   )




(define (get-limiter-for row-name)
  (case row-name
    ((BYMONTH) (limiter-positive-int (compose month datetime-date)))
    ((BYYEARDAY)
     (limiter-int (compose year-day datetime-date)
                  (compose days-in-year datetime-date)))
    ((BYMONTHDAY)
     (limiter-int (compose day datetime-date)
                  (compose days-in-month datetime-date)))
    ((BYDAY)
     ;; This is only relevant for WEEKLY, so we can safely assume that the
     ;; offset prefix is forbidden here, so for each spec we drop the prefix
     ;; TODO it's used in other contexts also, but with the same rules
     (lambda (_ rrule dt-list)
       ((limiter-positive-int (compose week-day datetime-date))
        (compose (cut map cdr <>) byday)
        rrule dt-list)))

    ((BYHOUR)   (limiter-positive-int (compose hour   datetime-time)))
    ((BYMINUTE) (limiter-positive-int (compose minute datetime-time)))
    ((BYSECOND) (limiter-positive-int (compose second datetime-time)))
    ((BYSETPOS) run-bysetpos)
    (else (scm-error 'misc-error "get-limiter-for"
                     "No limiter for ~s"
                     (list row-name) #f))))

(define (get-expander-for row-name week-start)
  (case row-name
    ((BYMONTH)
     (expander-int
      (lambda (month-no dt)
        (modify dt date*
                (lambda (d) (set d month* month-no))))))

    ((BYWEEKNO)
     (expander-int
      (lambda (week-no dt)
        (modify dt date*
                (lambda (d)
                  (date-starting-week (if (positive? week-no)
                                          week-no
                                          (- (weeks-in-year d) week-no))
                                      d week-start))))))

    ((BYYEARDAY)
     (expander-int
      (lambda (yearday dt)
        (modify dt date*
                (lambda (d)
                  (if (positive? yearday)
                      (date+ (start-of-year d)
                             (duration day: (1- yearday)))
                      (date- (date+ (start-of-year d) (date year: 1))
                             (duration day: (- yearday)))))))))

    ((BYMONTHDAY)
     (expander-int
      (lambda (monthday dt)
        (modify dt date*
                (lambda (d)
                  (set d day*
                       (if (positive? monthday)
                           monthday
                           (- (days-in-month d)
                              (1- (- monthday))))))))))

    ((BYDAY)
     ;; This is only relevant for WEEKLY, so we can safely assume that the
     ;; offset prefix is forbidden here, so for each spec we drop the prefix
     ;; TODO it's used in other contexts also, but with the same rules
     (lambda (_ rrule dt-list)
      ((expander-int
        (lambda (weekday dt)
          (modify dt date*
                  (lambda (d)
                    (date+ (start-of-week d week-start)
                           (duration day: (modulo (- weekday week-start) 7)))))))
       (compose (cut map cdr <>) byday)
       rrule dt-list)))

    ((BYHOUR)
     (expander-int (lambda (h dt) (set dt (lens-compose time* hour*) h))))

    ((BYMINUTE)
     (expander-int (lambda (m dt) (set dt (lens-compose time* minute*) m))))

    ((BYSECOND)
     (expander-int (lambda (m dt) (set dt (lens-compose time* minute*) m))))))




;; Base cases is now a stream of lists of datetime objects.
;; Each expander and limiter is given this list in turn, and MUST
;; return a new list of datetime objects, with corresponding entries
;; added or removed. The complete list of posibilities is then retrieved through
;; TODO it is possible for multiple seeds to generate the same rule!
;; For example FREQ=YEARLY;BYMONTH=1,2;BYWEEKNO=5;WKST=MO, with the
;; start of 2025-01-01:
;; BYMONTH expansion gives us
;;   (stream (list #2025-01-01 #2025-02-01) ...)
;; BYWEEKNO expansion gives us
;;   (stream (append (list #2025-01-27)
;;                   (list #2025-01-27))
;;           ...)
;; It's easy to fix in this case, but could two non-adjacent entries
;; be equal?

(define (rrule-instances start rrule)
  (typecheck start datetime?)
  (typecheck rrule recur-rule?)

  ;; eval FREQ and INTERVAL
  (define increment
    (case (freq rrule)
      ((SECONDLY) (duration second: (interval rrule)))
      ((MINUTELY) (duration minute: (interval rrule)))
      ((HOURLY)   (duration hour:   (interval rrule)))
      ((DAILY)    (duration day:    (interval rrule)))
      ((WEEKLY)   (duration week:   (interval rrule)))
      ((MONTHLY)  (duration month:  (interval rrule)))
      ((YEARLY)   (duration year:   (interval rrule)))
      (else (unreachable "evaluate-recurrence-set"
                         "Invalid recurrence rule frequency: ~s"
                         (list rrule)))))
  (define base-cases
    (->> start
         (stream-iterate (lambda (x) (datetime+ x increment)))
         (stream-map list)))

  ;; look up rule depending on freq
  (define expanded
    (stream-unique
     (stream-concat
      (stream-map
       (lambda (seed)
         (list->stream
          (fold (lambda (row-idx dt-list)
                  (define row-name (array-ref rrule-table row-idx 0))
                  (define accessor (assoc-ref rrule-accessors row-name))

                  (cond ((not (accessor rrule))
                         dt-list)
                        (else
                         (sort*
                          ((case (array-ref rrule-table row-idx (assoc-ref column-indices (freq rrule)))
                             ((Limit)  (get-limiter-for  row-name))
                             ((Expand) (get-expander-for row-name (wkst rrule)))
                             ((Note-1)
                              (cond ((bymonthday rrule)
                                     (get-limiter-for 'BYDAY))
                                    (else (byday-expander all-wday-in-month
                                                          start-of-month))))
                             ((Note-2)
                              (cond ((or (byyearday rrule) (bymonthday rrule))
                                     (get-limiter-for 'BYDAY))
                                    ((byweekno rrule)
                                     (get-expander-for 'BYDAY (wkst rrule)))
                                    ((bymonth rrule)
                                     (byday-expander all-wday-in-month
                                                     start-of-month))
                                    (else (byday-expander all-wday-in-year
                                                          start-of-year))))

                             ((N/A) (scm-error 'misc-error "evaluate-recurrence-set"
                                               "~a invalid for ~a frequency"
                                               (list row-name (freq rrule))
                                               #f))
                             (else
                              => (lambda (symb)
                                   (scm-error 'misc-error "evaluate-recurrence-set"
                                              "Unknown rule entry ~s, found at <~a, ~a>"
                                              (list symb (freq rrule) row-name)
                                              #f))))
                           (assoc-ref rrule-accessors row-name)
                           rrule dt-list)
                          datetime<=))))
                seed
                (let ((data-start 1))
                 (iota (- (car (array-dimensions rrule-table)) data-start) data-start)))
          ))
       base-cases))))

  (define limited-expanded
    (stream-drop-while (lambda (dt) (datetime< dt start)) expanded))

  ;; then finally apply COUNT and UNTIL
  (cond ((recur-count rrule) => (lambda (c) (stream-take c limited-expanded)))
        ((until rrule) => (lambda (u) (stream-take-while
                                  (lambda (dt)
                                    ;; TODO Exactly here
                                    ;; dt is unzoned, since we strip the zone
                                    ;; However, u might be zoned

                                    ;; Until is in local time iff dtstart is in localtime
                                    ;; and in UTC time otherwise
                                    (datetime<=
                                     dt (if (datetime? u)
                                            u
                                            (datetime date: u))))
                                  limited-expanded)))
        (else limited-expanded)))




(define (generate-recurrence-set/date base rest start duration)
  (typecheck base vevent?)
  (typecheck rest (list-of vevent?))

  (define exceptions (make-hash-table))
  (for component in rest
       (hash-set! exceptions (prop1 component 'RECURRENCE-ID) component))

  ;; TODO currently DATE values aren't expanded into pairs, since
  ;; the PERIOD type isn't applicable to DATE values.
  (chain
   (list (cond ((prop1 base 'RRULE)
                => (lambda (rrule)
                     ;; NOTE RFC 5545 seems to allow SECONDLY..HOURLY expanders
                     ;; even when DTSTART is of type DATE. We here treat that as
                     ;; undefined behaviour, and simply truncate everything back
                     ;; to pure DATE values.
                     (->> (rrule-instances (datetime date: start) rrule)
                          ;; Truncate back to dates
                          (stream-map datetime-date)
                          ;; Extra stream-unique required in case SECONDLY..HOURLY
                          ;; was present
                          stream-unique)))
               (else (stream)))

         (list->stream
          ;; We limit RDATEs to only be date instants here, since the PERIOD
          ;; type requires datetimes.
          ;; TODO codify this into the validator
          (cond ((prop% base 'RDATE)
                 => (lambda (rdates)
                      (sort* (map vline-value rdates)
                             date<)))
                (else '()))))
   (interleave-streams date< _)
   (stream-remove
    (lambda (d)
      (and=> (prop% base 'EXDATE)
             (lambda (vs) (member d (map vline-value vs)))))
    _)

   (stream-map
    (lambda (d)
      (or (hash-ref exceptions d)
          (-> base
              (set (prop* 'RECURRENCE-ID) (just (list (vline value: d))))
              (set (prop* 'DTSTART)       (just (list (vline value: d))))
              ;; Only add DTEND to generated instance if original instance had a DTEND
              (modify (prop* 'DTEND)
                      (destructure-lambda
                       ((nothing) (nothing))
                       ((just _) (just (list (vline value: (date+ d duration))))))))))
    _)
   ))


(define (generate-recurrence-set/zoned-datetime base rest start duration)
  (typecheck base vevent?)
  (typecheck rest (list-of vevent?))
  (typecheck start zoned-datetime?)
  (typecheck duration duration?)

  (define exceptions (make-hash-table))
  (for component in rest
       (hash-set! exceptions
                  ((unval zone->utc) (prop1 component 'RECURRENCE-ID))
                  component))

  (chain
   (list (cond ((prop1 base 'RRULE)
                => (lambda (rrule)
                     (stream-map
                      (lambda (dt) (cons dt (datetime+/zoneinfo dt duration)))
                      (rrule-instances start rrule))))
               (else (stream)))

         (list->stream
          (cond ((prop% base 'RDATE)
                 => (lambda (vlines)
                      (sort*
                       (map (lambda (vline)
                              (let ((v (vline-value vline)))
                                (cond ((datetime? v)
                                       (cons v (datetime+/zoneinfo v duration)))
                                      ((period? v)
                                       ;; The reference zone of
                                       ;; period->utc-datetimes
                                       ;; is only used for
                                       ;; unzoned datetimes.
                                       ;; Therefore, we chose a
                                       ;; non-existant one.
                                       ;; Numbers only to
                                       ;; make the error grep-able.
                                       (call-with-values
                                           (lambda () (period->utc-datetimes "UNUSED TZ, 61423" v))
                                         cons))
                                      (else (scm-error
                                             'misc-error "generate-recurrence-set"
                                             "Invalid RDATE value: ~s"
                                             (list v) #f)))))
                            vlines)
                       (compose datetime</zoneinfo car))))
                (else '()))))
   (interleave-streams (compose datetime</zoneinfo car) _)
   (stream-remove (lambda (p)
                    (cond ((prop% base 'EXDATE)
                           => (lambda (vs)
                                (find (lambda (v) (datetime=/zoneinfo (car p) v))
                                      (map vline-value vs))))
                          (else #f)))
                  _)

   (stream-map
    (lambda (p)
      (or (hash-ref exceptions ((unval zone->utc) (car p)))
          (-> base
              (set (prop* 'RECURRENCE-ID) (just (list (vline value: (car p)))))
              (set (prop* 'DTSTART)       (just (list (vline value: (car p)))))
              ;; Only add DTEND to generated instance if original instance had a DTEND
              (modify (prop* 'DTEND)
                      (destructure-lambda
                       ((nothing) (nothing))
                       ((just _) (just (list (vline value: (cdr p))))))))))
    _)
   ))


(define (generate-recurrence-set/unzoned-datetime base rest start duration)
  (define exceptions (make-hash-table))
  (for component in rest
       (hash-set! exceptions (prop1 component 'RECURRENCE-ID)
                  component))

  (chain
   (list (cond ((prop1 base 'RRULE)
                => (lambda (rrule)
                     ;; Project onto UTC, since UTC datetime
                     ;; arithmetic is identical to unzoned datetime
                     ;; arithmetic.
                     (stream-map (lambda (v) (cons v (datetime+/naive v duration)))
                                 (rrule-instances start rrule))))
               (else (stream)))

         (list->stream
          (cond ((prop% base 'RDATE)
                 => (lambda (rdates)
                      (sort*
                       (map
                        (lambda (vline)
                          (let ((v (vline-value vline)))
                            (cond ((datetime? v)
                                   (let ((v (tz v #f)))
                                     (cons v (datetime+/naive v duration))))
                                  ((period? v)
                                   (call-with-values (lambda () (period->utc-datetimes "UTC" v))
                                     (lambda (s e)
                                       (cons (tz s #f)
                                             (tz e #f)))))
                                  (else (scm-error
                                         'misc-error "generate-recurrence-set"
                                         "Invalid RDATE value: ~s"
                                         (list v) #f)))))
                        rdates)
                       datetime<)))
                (else '()))))

   (interleave-streams (compose datetime< car) _)

   (stream-remove
    (lambda (p)
      (and=> (prop% base 'EXDATE)
             (lambda (vs) (member (car p) (map vline-value vs)))))
    _)

   (stream-map
    (lambda (p)
      (or (hash-ref exceptions (car p))
          (-> base
              (set (prop* 'RECURRENCE-ID) (just (list (vline value: (car p)))))
              (set (prop* 'DTSTART)       (just (list (vline value: (car p)))))
              ;; Only add DTEND to generated instance if original instance had a DTEND
              (modify (prop* 'DTEND)
                      (destructure-lambda
                       ((nothing) (nothing))
                       ((just _) (just (list (vline value: (cdr p))))))))))
    _)
   ))




;;; date-table, mapping dates to objects

;;; datetime-table, mapping datetimes to objects
;;; unzoned can only map to unzoned, while zoned map regardless of zone
;;; (e.g. #2026-02-24T09:50 CET === #2026-02-24T08:50 UTC

;;; Returns a stream of vevent instances, where each vevent is one recurrence
;;; instance of the given event.
;;; DTSTART is updated, and DTEND where applicable.
;;; EXDATE and RDATES are handled
(define (generate-recurrence-set component)
  (typecheck component vcalendar?)
  ;; TODO
  ;; - SEQUENCE

  ;; find base event
  (define-values (base rest) (find-base-instance component))

  ;; Duration of event, when the base has a DTEND value.
  ;; DURATION values are ignored, since those are carried through automatically.

  ;; TODO Write tests for what happens when we pass timezone boundries in different ways.
  (define start (prop1 base 'DTSTART))
  (define duration (instance-length base))

  ;; -------------------- TODO LINE --------------------

  ;; (format (current-error-port) "base: ~s~%rest: ~s~%" base rest)

  ;; TODO for these cases, DTEND should only be added if the original instance had a DTEND

  (cond
   ((date? start)
    (generate-recurrence-set/date base rest start duration))

   ((zoned-datetime? start)
    (generate-recurrence-set/zoned-datetime base rest start duration))

   ((unzoned-datetime? start)
    (generate-recurrence-set/unzoned-datetime base rest start duration))

   (else (scm-error 'misc-error "generate-recurrence-set"
                    "Invalid type for dtstart: ~s"
                    (list start) #f)))

  ;; -------------------- TODO LINE --------------------
)


;; Takes a time interval in @var{start} and @var{end}, and the
;; complete set of recurring and non-recurring events in a calendar set.
;; 
;; The set of regular events MUST be a list of pairs of href strings
;; and vcalendar objects, each containing a single vevent
;; object. These objects will be sorted inside this procedure.
;; 
;; The set of recurring events have the same form as the regular events,
;; except that multile VEVENT components may be present. However, all
;; VEVENT components MUST refer to the same logical event.
;; 
;; Returns a stream of pairs, each containing the href of the entry
;; (so may not be unique), and the vcalendar instance, but with only a
;; single (expanded) instance. These events are sorted by their start date.
(define ((expand-and-interleave-recurrences reference-zone start end) recurring regular)
  (typecheck start zoned-datetime?)
  (typecheck end   zoned-datetime?)
  (typecheck recurring (list-of (pair-of string? vcalendar?)))
  (typecheck regular   (list-of (pair-of string? vcalendar?)))

  (format (current-error-port) "start: ~s, end: ~s, recurring: ~a, regular: ~a~%"
          start end (length recurring) (length regular))

  

  (define utc-start ((unval zone->utc) start))
  (define utc-end   ((unval zone->utc) end))

  ;; (define fake-start (tz start #f))
  ;; (define fake-end   (tz end   #f))


  (define non-recurring-instances
   (chain regular
          (map (lambda (pair)
                 (define-values (href cal) (car+cdr pair))
                 (define instance (car (vcomponent-children cal)))
                 (define local-start (instance-start-datetime reference-zone instance))
                 (define utc-start ((unval zone->utc) local-start))
                 (define utc-end ((unval zone->utc)
                                  (datetime+/zoneinfo local-start (instance-length instance))))
                 (vector utc-start utc-end href cal))
               _)
          (filter (lambda (record)
                    (timespan-overlaps? utc-start utc-end
                                        (vector-ref record 0)
                                        (vector-ref record 1)))
                  _)
          (sort* _ datetime< (lambda (record) (vector-ref record 0)))
          (list->stream _)))

  (define recurring-instances
    (for (href . cal) in recurring
         (stream-map
          (lambda (instance)
            ;; (define local-start (instance-start-datetime reference-zone instance))
            ;; (define utc-start ((unval zone->utc) local-start))
            ;; (define utc-end ((unval zone->utc)
            ;;                  (datetime+/zoneinfo local-start (instance-length instance))))
            (define fake-start (ensure-zoned-datetime "UTC" (prop1 instance 'DTSTART)))
            (define fake-end (datetime+ fake-start (instance-length instance)))
            (vector
             ;; utc-start utc-end
             fake-start fake-end
             href
             (-> cal (vcomponent-children (list instance)))))
          (generate-recurrence-set cal))))

  
;; 

;;   (stream-filter
;;    (lambda (p)
;;      (let ((ev (car (vcomponent-children (cdr p)))))
;;        ;; TODO
;;        ;; (instance-overlaps? reference-zone ev start end)
;;        (timespan-overlaps? fake-start fake-end
;;                            (ensure-zoned-datetime "UTC" (prop1 ev 'DTSTART))
;;                            (ensure-zoned-datetime "UTC" (prop1 ev 'DTEND)))
;;        ))
;;    (stream-take-while
;;     (lambda (p)
;;       ;; (datetime</zoneinfo
;;       ;;  (instance-start-datetime reference-zone (car (vcomponent-children (cdr p))))
;;       ;;  end)
;;       (datetime<
;;        (ensure-zoned-datetime "UTC" (prop1 (car (vcomponent-children (cdr p))) 'DTSTART))
;;        fake-end)
;;       )
;;     (interleave-streams
;;      ;; (lambda (a b) (datetime</zoneinfo (instance-start-datetime reference-zone (car (vcomponent-children (cdr a))))
;;      ;;                              (instance-start-datetime reference-zone (car (vcomponent-children (cdr b))))))
;;      ;; TODO TODO
;;      ;; This is a temporary measure to speed up stuff
;;      (lambda (a b) (datetime< (ensure-zoned-datetime "UTC" (prop1 (car (vcomponent-children (cdr a))) 'DTSTART))
;;                          (ensure-zoned-datetime "UTC" (prop1 (car (vcomponent-children (cdr b))) 'DTSTART))))
;;      (cons
;;       non-recurring-instances
;;       recurring-instances))))

  


  

  (case 2
   ((1)
    ;; - interleave all recurrences
    ;; - filter
    ;; - filter more
    ;; - normalize to old return format
    (chain
     (cons non-recurring-instances recurring-instances)

     (interleave-streams
      (lambda (a b) (datetime< (vector-ref a 0) (vector-ref b 0)))
      _)

     (stream-take-while
      (lambda (p) (datetime< (vector-ref p 0) utc-end))
      _)

     ;; filter-sorted-stream fails if the first element of the set is after our time
     (stream-filter
      (lambda (p) (timespan-overlaps? utc-start utc-end (vector-ref p 0) (vector-ref p 1)))
      _)


     ))
   ((2)
    (chain
     ;; - filter
     ;; - filter more
     ;; - interleave recurrences
     ;; - normalize to old return format
     (cons non-recurring-instances recurring-instances)
     (map (lambda (strm)
            (->> strm
                 (stream-take-while
                  (lambda (p) (datetime< (vector-ref p 0) utc-end)))
                 (stream-filter
                  (lambda (p) (timespan-overlaps? utc-start utc-end (vector-ref p 0) (vector-ref p 1))))
                 ))
          _)


     (interleave-streams
      (lambda (a b) (datetime< (vector-ref a 0) (vector-ref b 0)))
      _)

     ;; (list->stream _)
     ;; (stream-concat _)

     ;; (stream-map
     ;;  (lambda (record) (cons (vector-ref record 2) (vector-ref record 3)))
     ;;  _)

     )
    )
   ))

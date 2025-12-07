(define-module (vcomponent type recurrence generate)
  :use-module (vcomponent)
  :use-module (vcomponent type recurrence internal)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util exceptions)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 curried-definitions)
  :export (find-base-instance
           generate-recurrence-set
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
  (define focus
    (or (find (lambda (component)
                (or (prop1 component 'RRULE)
                    (prop1 component 'RDATE)))
              (vcomponent-children event))
        (find-extreme
         (vcomponent-children event)
         datetime<
         (compose as-datetime (extract1 'DTSTART)))))

  (values focus (delq focus (vcomponent-children event))))




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
  (filter (lambda (dt) (datetime= dt (datetime+ dt (datetime))))
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
                             (date day: (1- yearday)))
                      (date- (date+ (start-of-year d) (date year: 1))
                             (date day: (- yearday)))))))))

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
                           (date day: (modulo (- weekday week-start) 7)))))))
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
      ((SECONDLY) (datetime second:   (interval rrule)))
      ((MINUTELY) (datetime minute:   (interval rrule)))
      ((HOURLY)   (datetime hour:     (interval rrule)))
      ((DAILY)    (datetime day:      (interval rrule)))
      ((WEEKLY)   (datetime day: (* 7 (interval rrule))))
      ((MONTHLY)  (datetime month:    (interval rrule)))
      ((YEARLY)   (datetime year:     (interval rrule)))
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
                                  (lambda (dt) (datetime<= dt (as-datetime u)))
                                  limited-expanded)))
        (else limited-expanded)))


(define (generate-recurrence-set component)
  (typecheck component vcalendar?)
  ;; TODO
  ;; - SEQUENCE

  ;; find base event
  (define-values (base rest) (find-base-instance component))
  ;; Make note of all exceptions.
  (define recurrence-id-exceptions
   (fold (lambda (component rec-id-table)
           (cond ((prop% component 'RECURRENCE-ID)
                  => (lambda (rid)
                       ;; TODO parameter RANGE=THISANDFUTURE
                       (table-put rec-id-table
                                  (-> rid car vline-value
                                      as-datetime datetime->string
                                      string->symbol)
                                  component)))
                 (else rec-id-table)))
         (table)
         rest))

  ;; Duration of event, when the base has a DTEND value.
  ;; DURATION values are ignored, since those are carried through automatically.
  (define duration
    (and=> (prop1 base 'DTEND)
           (lambda (end) (datetime-difference (as-datetime end)
                                         (as-datetime (prop1 base 'DTSTART))))))

  (stream-map
   (lambda (dt)
     (or (table-get recurrence-id-exceptions (-> dt datetime->string string->symbol))
         (-> base
             (set (prop* 'DTSTART)
                  (just (list (vline value: (if (datetime? (prop1 base 'DTSTART))
                                                dt (datetime-date dt))))))
             (set (prop* 'DTEND)
                  (if duration
                      (just (list (vline value:
                                         (let ((end (datetime+ dt duration)))
                                           (if (datetime? (prop1 base 'DTSTART))
                                               end (datetime-date end))))))
                      (nothing))))))

   ;; If EXDATE exists, omit those entries
   (stream-remove
    (lambda (dt) (member dt (or (map (compose as-datetime vline-value) (or (prop% base 'EXDATE) '())))))
    (interleave-streams
     datetime<
     (list
      ;; if rdate exists, sort these and put them into a stream
      ;; NOTE that rdats amy be datetime?, date?, or period?
      (list->stream (sort* (map (compose as-datetime vline-value) (or (prop% base 'RDATE) '()))
                           datetime<))
      ;; (if rrule exists, run rrule-instances)
      (rrule-instances
       (as-datetime (prop1 base 'DTSTART))
       (prop1 base 'RRULE)))))))

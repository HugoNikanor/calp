(define-module (vcomponent datetime)
  #:use-module (srfi srfi-1)
  #:use-module (vcomponent base)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (datetime zic)
  #:use-module (util)

  :use-module (ice-9 curried-definitions)

  #:export (#;parse-datetime
            event-overlaps?
            overlapping?
            event-contains?
            ev-time<?)
  )

;;; date time pointer
#;
(define (parse-datetime dtime)
  "Parse the given date[time] string into a date object."
  (string->date
   dtime (case (string-length dtime)
           ((8)  "~Y~m~d")              ; All day
           ((15) "~Y~m~dT~H~M~S")       ; "local" or TZID-param
           ((16) "~Y~m~dT~H~M~S~z"))))  ; UTC-time

(define (event-overlaps? event begin end)
  "Returns if the event overlaps the timespan.
Event must have the DTSTART and DTEND attribute set."
  (timespan-overlaps? (attr event 'DTSTART)
                      (or (attr event 'DTEND) (attr event 'DTSTART))
                      begin end))

(define (overlapping? event-a event-b)
  (timespan-overlaps? (attr event-a 'DTSTART)
                      (or (attr event-a 'DTEND)
                          (if (date? (attr event-a 'DTSTART))
                              (date+ (attr event-a 'DTSTART) (date day: 1))
                              (attr event-a 'DTSTART)))
                      (attr event-b 'DTSTART)
                      (or (attr event-b 'DTEND)
                          (if (date? (attr event-b 'DTSTART))
                              (date+ (attr event-b 'DTSTART) (date day: 1))
                              (attr event-b 'DTSTART)))))

(define (event-contains? ev date/-time)
  "Does event overlap the date that contains time."
  (let* ((start (as-date date/-time))
         (end (add-day start)))
    (event-overlaps? ev start end)))

(define-public (event-zero-length? ev)
  (and (datetime? (attr ev 'DTSTART))
       (not (attr ev 'DTEND))))

(define-public (ev-time<? a b)
  (date/-time<? (attr a 'DTSTART)
                (attr b 'DTSTART)))

;; Returns length of the event @var{e}, as a time-duration object.
(define-public (event-length e)
  (if (not (attr e 'DTEND))
      (if (date? (attr e 'DTSTART))
          (date day: 1)
          (datetime))
      ((if (date? (attr e 'DTSTART))
           date-difference datetime-difference)
       (attr e 'DTEND) (attr e 'DTSTART))))

(define-public (event-length/clamped start-date end-date e)
  (let ((end (or (attr e 'DTEND)
                 (if (date? (attr e 'DTSTART))
                     (date+ (attr e 'DTSTART) (date day: 1))
                     (attr e 'DTSTART)))))
    (if (date? (attr e 'DTSTART))
        (date-difference (date-min (date+ end-date (date day: 1))
                                   end)
                         (date-max start-date
                                   (attr e 'DTSTART)))
        (datetime-difference (datetime-min (datetime date: (date+ end-date (date day: 1)))
                                           end)
                             (datetime-max (datetime date: start-date)
                                           (attr e 'DTSTART))))))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
;; currently the secund argument is a date, but should possibly be changed
;; to a datetime to allow for more explicit TZ handling?
(define-public (event-length/day date e)
  (if (not (attr e 'DTEND))
      (if (date? (attr e 'DTSTART))
          #24:00:00
          (time))
      (let ((start (attr e 'DTSTART))
            (end (attr e 'DTEND)))
        (cond [(date= date (as-date start) (as-date end))
               (time- (as-time end) (as-time start))]
              ;; Starts today, end in future day
              [(date= date (as-date start))
               (time- #24:00:00 (as-time start))]
              ;; Ends today, start earlier day
              [(date= date (as-date end))
               (as-time end)]
              ;; start earlier date, end later date
              [else #24:00:00]))))


;; 22:00 - 03:00
;; 2h för dag 1
;; 3h för dag 2

;; An event is considered long if it's DTSTART (and thereby DTEND) lacks a time component,
;; or if the total length of the event is greater than 24h.
;; For practical purposes, an event being long means that it shouldn't be rendered as a part
;; of a regular day.
(define-public (long-event? ev)
  (if (date? (attr ev 'DTSTART))
      #t
      (aif (attr ev 'DTEND)
           (datetime<= (datetime date: (date day: 1))
                       (datetime-difference it (attr ev 'DTSTART)))
           #f)))


;; DTEND of the last instance of this event.
;; event → (or datetime #f)
(define (final-spanned-time event)
  (if (not ((@ (vcomponent recurrence) repeating?) event))
      (or (attr event 'DTEND) (attr event 'DTSTART))
      (let ((final ((@ (vcomponent recurrence generate) final-event-occurence)
                    event)))
        (if final
            (aif (attr event 'DTEND)
                 (datetime+ (as-datetime final) (as-datetime it))
                 (as-datetime final))
            #f))))

;; date, date, [sorted-stream events] → [list events]
(define-public (events-between start-date end-date events)
  (define (overlaps e)
    (timespan-overlaps? start-date (date+ end-date (date day: 1))
                        (attr e 'DTSTART) (or (attr e 'DTEND)
                                              (attr e 'DTSTART))))

  ((@ (srfi srfi-41) stream-filter)
   overlaps
   ((@ (srfi srfi-41 util) get-stream-interval)
    overlaps
    (lambda (e) (not (date< end-date (as-date (attr e 'DTSTART)))))
    events)))





;; Checks if the given zone-entry is relevant for this event
;; by checking if zone-entry-until isn't before our DTSTART.
(define ((relevant-zone-entry? event) zone-entry)
  (aif (zone-entry-until zone-entry)
       (datetime<? (as-datetime (attr event 'DTSTART)) it)
       #t))

(define ((relevant-zone-rule? event) rule)
  (define start (attr event 'DTSTART))
  ;; end := datetime | #f
  (define end (final-spanned-time event))

  (define start-y (year (as-date start)))

  (if end
      (let ((end-y (and end (year (as-date end)))))
        (cond [(and (eq? 'minimum (rule-from rule))
                    (eq? 'maximum (rule-to rule)))
               #t]
              [(eq? 'minimum (rule-from rule))
               (< start-y (rule-to rule))]
              [(eq? 'maximum (rule-to rule))
               (< (rule-from rule) end-y)]
              [(eq? 'only (rule-to rule))
               (<= start-y (rule-from rule) end-y)]
              [else
               (timespan-overlaps? start end
                                   (date year: (rule-from rule))
                                   (date year: (1+ (rule-to rule))))]))
      (cond [(and (eq? 'minimum (rule-from rule))
                  (eq? 'maximum (rule-to rule)))
             #t]
            [(eq? 'minimum (rule-from rule))
             (< start-y (rule-to rule))]
            [(eq? 'maximum (rule-to rule))
             #t]
            [(eq? 'only (rule-to rule))
             (<= start-y (rule-from rule))]
            [else
             (<= (rule-from rule) start-y (rule-to rule))])))

;; event is for limiter
(define-public (zoneinfo->vtimezone zoneinfo zone-name event)
  (define vtimezone (make-vcomponent 'VTIMEZONE))
  (define last-until (datetime date: #1000-01-01))
  (define last-offset (timespec-zero))
  (set! (attr vtimezone 'TZID) zone-name)

  (for zone-entry in (filter (relevant-zone-entry? event) (get-zone zoneinfo zone-name))
       (cond [(zone-entry-rule zone-entry) timespec?
              => (lambda (inline-rule)
                   (let ((component (make-vcomponent 'DAYLIGHT))
                         (new-timespec (timespec-add
                                        (zone-entry-stdoff zone-entry)
                                        inline-rule)))
                     (set! (attr component 'DTSTART) last-until
                           (attr component 'TZOFFSETFROM) last-offset
                           (attr component 'TZOFFSETTO) new-timespec
                           (attr component 'TZNAME) (zone-entry-format zone-entry)
                           last-until (zone-entry-until zone-entry)
                           last-offset new-timespec)
                     (add-child! vtimezone component)))]

             [(zone-entry-rule zone-entry)
              => (lambda (rule-name)
                   (map (lambda (rule)
                          (let ((component (make-vcomponent
                                            ;; NOTE the zoneinfo database doesn't
                                            ;; come with information if a given
                                            ;; rule is in standard or daylight time,
                                            ;; since that's mostly nonsencical
                                            ;; (e.g. war- and peacetime).
                                            ;; But the ical standard requires that,
                                            ;; so this is a fair compromize.
                                            (if (string-null? (rule-letters rule))
                                                'STANDARD 'DAYLIGHT)))
                                (new-timespec (timespec-add
                                               (zone-entry-stdoff zone-entry)
                                               (rule-save rule))))

                            (set! (attr component 'DTSTART) (rule->dtstart rule)
                                  (attr component 'TZOFFSETFROM) last-offset
                                  (attr component 'TZOFFSETTO) new-timespec
                                  (attr component 'TZNAME) (zone-format
                                                            (zone-entry-format zone-entry)
                                                            (rule-letters rule))
                                  ;; NOTE this can both be a number or the
                                  ;; symbol 'maximum
                                  last-until (zone-entry-until zone-entry)
                                  last-offset new-timespec)

                            (awhen (rule->rrule rule)
                                   (set! (attr component 'RRULE) it))

                            (add-child! vtimezone component)))
                        ;; some of the rules might not apply to us since we only
                        ;; started using that rule set later. It's also possible
                        ;; that we stopped using a ruleset which continues existing.
                        ;;
                        ;; Both these are filtered here.
                        (filter
                         (relevant-zone-rule? event)
                         (get-rule zoneinfo rule-name))))]

             [else                      ; no rule
              (let ((component (make-vcomponent 'STANDARD)))
                ;; DTSTART MUST be a datetime in local time
                (set! (attr component 'DTSTART) last-until
                      (attr component 'TZOFFSETFROM) last-offset
                      (attr component 'TZOFFSETTO) (zone-entry-stdoff zone-entry)
                      (attr component 'TZNAME) (zone-entry-format zone-entry)
                      last-until (zone-entry-until zone-entry)
                      last-offset (zone-entry-stdoff zone-entry))
                (add-child! vtimezone component))]))
  vtimezone)

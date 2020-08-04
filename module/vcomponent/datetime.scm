(define-module (vcomponent datetime)
  #:use-module (srfi srfi-1)
  #:use-module (vcomponent base)
  #:use-module (datetime)
  #:use-module (datetime timespec)
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
Event must have the DTSTART and DTEND protperty set."
  (timespan-overlaps? (prop event 'DTSTART)
                      (or (prop event 'DTEND) (prop event 'DTSTART))
                      begin end))

(define (overlapping? event-a event-b)
  (timespan-overlaps? (prop event-a 'DTSTART)
                      (or (prop event-a 'DTEND)
                          (if (date? (prop event-a 'DTSTART))
                              (date+ (prop event-a 'DTSTART) (date day: 1))
                              (prop event-a 'DTSTART)))
                      (prop event-b 'DTSTART)
                      (or (prop event-b 'DTEND)
                          (if (date? (prop event-b 'DTSTART))
                              (date+ (prop event-b 'DTSTART) (date day: 1))
                              (prop event-b 'DTSTART)))))

(define (event-contains? ev date/-time)
  "Does event overlap the date that contains time."
  (let* ((start (as-date date/-time))
         (end (add-day start)))
    (event-overlaps? ev start end)))

(define-public (event-zero-length? ev)
  (and (datetime? (prop ev 'DTSTART))
       (not (prop ev 'DTEND))))

(define-public (ev-time<? a b)
  (date/-time<? (prop a 'DTSTART)
                (prop b 'DTSTART)))

;; Returns length of the event @var{e}, as a time-duration object.
(define-public (event-length e)
  (if (not (prop e 'DTEND))
      (if (date? (prop e 'DTSTART))
          (date day: 1)
          (datetime))
      ((if (date? (prop e 'DTSTART))
           date-difference datetime-difference)
       (prop e 'DTEND) (prop e 'DTSTART))))

(define-public (event-length/clamped start-date end-date e)
  (let ((end (or (prop e 'DTEND)
                 (if (date? (prop e 'DTSTART))
                     (date+ (prop e 'DTSTART) (date day: 1))
                     (prop e 'DTSTART)))))
    (if (date? (prop e 'DTSTART))
        (date-difference (date-min (date+ end-date (date day: 1))
                                   end)
                         (date-max start-date
                                   (prop e 'DTSTART)))
        (datetime-difference (datetime-min (datetime date: (date+ end-date (date day: 1)))
                                           end)
                             (datetime-max (datetime date: start-date)
                                           (prop e 'DTSTART))))))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
;; currently the secund argument is a date, but should possibly be changed
;; to a datetime to allow for more explicit TZ handling?
(define-public (event-length/day date e)
  (if (not (prop e 'DTEND))
      (if (date? (prop e 'DTSTART))
          (time hour: 24)
          (time))
      (let ((start (prop e 'DTSTART))
            (end (prop e 'DTEND)))
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
(define-public (long-event? ev)
  (if (date? (prop ev 'DTSTART))
      #t
      (aif (prop ev 'DTEND)
           (datetime<= (datetime date: (date day: 1))
                       (datetime-difference it (prop ev 'DTSTART)))
           #f)))

(define-public (really-long-event? ev)
  (let ((start (prop ev 'DTSTART))
        (end (prop ev 'DTEND)))
   (if (date? start)
       (and end (date< (date+ start (date day: 1)) end))
       (and end
            (datetime< (datetime date: (date day: 1))
                       (datetime-difference end start))))))


;; DTEND of the last instance of this event.
;; event → (or datetime #f)
(define (final-spanned-time event)
  (if (not ((@ (vcomponent recurrence) repeating?) event))
      (or (prop event 'DTEND) (prop event 'DTSTART))
      (let ((final ((@ (vcomponent recurrence generate) final-event-occurence)
                    event)))
        (if final
            (aif (prop event 'DTEND)
                 (datetime+ (as-datetime final) (as-datetime it))
                 (as-datetime final))
            #f))))

;; date, date, [sorted-stream events] → [sorted-stream events]
(define-public (events-between start-date end-date events)
  (define (overlaps e)
    (timespan-overlaps? start-date (date+ end-date (date day: 1))
                        (prop e 'DTSTART) (or (prop e 'DTEND)
                                              (prop e 'DTSTART))))

  ((@ (srfi srfi-41) stream-filter)
   overlaps
   ((@ (srfi srfi-41 util) get-stream-interval)
    overlaps
    (lambda (e) (not (date< end-date (as-date (prop e 'DTSTART)))))
    events)))





;; Checks if the given zone-entry is relevant for this event
;; by checking if zone-entry-until isn't before our DTSTART.
(define ((relevant-zone-entry? event) zone-entry)
  (aif (zone-entry-until zone-entry)
       (datetime<? (as-datetime (prop event 'DTSTART)) it)
       #t))

(define ((relevant-zone-rule? event) rule)
  (define start (prop event 'DTSTART))
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
  (define last-until (datetime date: (date month: 1 day: 1)))
  (define last-offset (timespec-zero))
  (set! (prop vtimezone 'TZID) zone-name)

  (for zone-entry in (filter (relevant-zone-entry? event) (get-zone zoneinfo zone-name))
       (cond [(zone-entry-rule zone-entry) timespec?
              => (lambda (inline-rule)
                   (let ((component (make-vcomponent 'DAYLIGHT))
                         (new-timespec (timespec-add
                                        (zone-entry-stdoff zone-entry)
                                        inline-rule)))
                     (set! (prop component 'DTSTART) last-until
                           (prop component 'TZOFFSETFROM) last-offset
                           (prop component 'TZOFFSETTO) new-timespec
                           (prop component 'TZNAME) (zone-entry-format zone-entry)
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

                            (set! (prop component 'DTSTART) (rule->dtstart rule)
                                  (prop component 'TZOFFSETFROM) last-offset
                                  (prop component 'TZOFFSETTO) new-timespec
                                  (prop component 'TZNAME) (zone-format
                                                            (zone-entry-format zone-entry)
                                                            (rule-letters rule))
                                  ;; NOTE this can both be a number or the
                                  ;; symbol 'maximum
                                  last-until (zone-entry-until zone-entry)
                                  last-offset new-timespec)

                            (awhen (rule->rrule rule)
                                   (set! (prop component 'RRULE) it))

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
                (set! (prop component 'DTSTART) last-until
                      (prop component 'TZOFFSETFROM) last-offset
                      (prop component 'TZOFFSETTO) (zone-entry-stdoff zone-entry)
                      (prop component 'TZNAME) (zone-entry-format zone-entry)
                      last-until (zone-entry-until zone-entry)
                      last-offset (zone-entry-stdoff zone-entry))
                (add-child! vtimezone component))]))
  vtimezone)

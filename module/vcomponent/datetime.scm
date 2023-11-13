(define-module (vcomponent datetime)
  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-41) :select (stream-filter))
  :use-module ((srfi srfi-41 util) :select (get-stream-interval))
  :use-module (vcomponent base)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (datetime zic)
  :use-module (hnh util)
  :use-module ((vcomponent recurrence generate)
               :select (final-event-occurence))
  :use-module (ice-9 curried-definitions)

  :export (#;parse-datetime
           event-overlaps?
           overlapping?
           event-contains?
           event-zero-length?
           ev-time<?

           event-length
           event-length/clamped
           event-length/day

           long-event?
           really-long-event?

           events-between

           zoneinfo->vtimezone
           ))

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
         (end (date+ start (date day: 1))))
    (event-overlaps? ev start end)))

(define (event-zero-length? ev)
  (and (datetime? (prop ev 'DTSTART))
       (not (prop ev 'DTEND))))

(define (ev-time<? a b)
  (date/-time<? (prop a 'DTSTART)
                (prop b 'DTSTART)))

;; Returns length of the event @var{e}, as a time-duration object.
(define (event-length e)
  (if (not (prop e 'DTEND))
      (if (date? (prop e 'DTSTART))
          (date day: 1)
          (datetime))
      ((if (date? (prop e 'DTSTART))
           date-difference
           datetime-difference)
       (prop e 'DTEND)
       (prop e 'DTSTART))))

;;
;; |-----|      extent of event
;;     |-----|  time we are interested in,
;;              defined through @var{start-date} and @var{end-date}
;;     |X|      part of event within that time (X)
;; 
;; Returns the length of the interval (X).
(define (event-length/clamped start-date end-date e)
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
(define (event-length/day date e)
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
(define (long-event? ev)
  (if (date? (prop ev 'DTSTART))
      #t
      (aif (prop ev 'DTEND)
           (datetime<= (datetime day: 1)
                       (datetime-difference it (prop ev 'DTSTART)))
           #f)))

(define (really-long-event? ev)
  (let ((start (prop ev 'DTSTART))
        (end (prop ev 'DTEND)))
    (and end (if (date? start)
                 (date< (date+ start (date day: 1)) end)
                 (datetime< (datetime day: 1)
                            (datetime-difference end start))))))


;; DTEND of the last instance of this event.
;; event → (or datetime #f)
(define (final-spanned-time event)
  (if (not ((@ (vcomponent recurrence) repeating?) event))
      (or (prop event 'DTEND) (prop event 'DTSTART))
      (let ((final (final-event-occurence event)))
        (if final
            (aif (prop event 'DTEND)
                 (datetime+ (as-datetime final) (as-datetime it))
                 (as-datetime final))
            #f))))

;; date, date, [sorted-stream events] → [sorted-stream events]
(define (events-between start-date end-date events)
  (define (overlaps e)
    (timespan-overlaps? start-date (date+ end-date (date day: 1))
                        (prop e 'DTSTART) (or (prop e 'DTEND)
                                              (prop e 'DTSTART))))

  (stream-filter
   overlaps
   (get-stream-interval
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
(define (zoneinfo->vtimezone zoneinfo zone-name event)
  (define last-until (datetime date: (date month: 1 day: 1)))
  (define last-offset (timespec-zero))

  (fold (lambda (zone-entry vtimezone)
          (cond [(zone-entry-rule zone-entry) timespec?
                 => (lambda (inline-rule)
                      (let ((component (vcomponent type: 'DAYLIGHT))
                            (new-timespec (timespec-add
                                           (zone-entry-stdoff zone-entry)
                                           inline-rule)))
                        (let ((component
                               (set-properties
                                component
                                (cons 'DTSTART last-until)
                                (cons 'TZOFFSETFROM last-offset)
                                (cons 'TZOFFSETTO new-timespec)
                                (cons 'TZNAME (zone-entry-format zone-entry)))))
                          (set! last-until (zone-entry-until zone-entry)
                                last-offset new-timespec)
                          (add-child vtimezone component))))]

                [(zone-entry-rule zone-entry)
                 => (lambda (rule-name)
                      (fold (lambda (rule vtimezone)
                              (let ((component (vcomponent
                                                type:
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

                                (let ((component
                                       (set-properties
                                        component
                                        (cons 'DTSTART (rule->dtstart rule))
                                        (cons 'TZOFFSETFROM last-offset)
                                        (cons 'TZOFFSETTO new-timespec)
                                        (cons 'TZNAME (zone-format
                                                       (zone-entry-format zone-entry)
                                                       (rule-letters rule))))))

                                  (set! ;; NOTE this can both be a number or the
                                      ;; symbol 'maximum
                                      last-until (zone-entry-until zone-entry)
                                      last-offset new-timespec)

                                  (add-child
                                   vtimezone
                                   (cond ((rule->rrule rule)
                                          => (lambda (it) (prop component 'RRULE it)))
                                         (else component))))))
                            vtimezone
                           ;; some of the rules might not apply to us since we only
                           ;; started using that rule set later. It's also possible
                           ;; that we stopped using a ruleset which continues existing.
                           ;;
                           ;; Both these are filtered here.
                           (filter
                            (relevant-zone-rule? event)
                            (get-rule zoneinfo rule-name))))]

                [else                      ; no rule
                 ;; DTSTART MUST be a datetime in local time
                 (let ((component
                        (set-properties
                         (vcomponent type: 'STANDARD)
                         (cons 'DTSTART last-until)
                         (cons 'TZOFFSETFROM last-offset)
                         (cons 'TZOFFSETTO (zone-entry-stdoff zone-entry))
                         (cons 'TZNAME (zone-entry-format zone-entry)))))
                   (set! last-until (zone-entry-until zone-entry)
                         last-offset (zone-entry-stdoff zone-entry))
                   (add-child vtimezone component))
                 ])
          )
        (prop (vcomponent type: 'VTIMEZONE) 'TZID zone-name)
        (filter (relevant-zone-entry? event)
                (get-zone zoneinfo zone-name))
        ))

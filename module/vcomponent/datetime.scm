(define-module (vcomponent datetime)
  #:use-module (srfi srfi-1)
  #:use-module (vcomponent base)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (datetime zic)
  #:use-module (util)

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
                      (attr event 'DTEND)
                      begin end))

(define (overlapping? event-a event-b)
  (timespan-overlaps? (attr event-a 'DTSTART)
                      (attr event-a 'DTEND)
                      (attr event-b 'DTSTART)
                      (attr event-b 'DTEND)))

(define (event-contains? ev date/-time)
  "Does event overlap the date that contains time."
  (let* ((start (as-date date/-time))
         (end (add-day start)))
    (event-overlaps? ev start end)))

(define-public (ev-time<? a b)
  (date/-time<? (attr a 'DTSTART)
                (attr b 'DTSTART)))

;; Returns length of the event @var{e}, as a time-duration object.
(define-public (event-length e)
  (if (not (attr e 'DTEND))
      (datetime date:
                (if (date? (attr e 'DTSTART))
                    #24:00:00
                    #01:00:00))
      ((if (date? (attr e 'DTSTART))
           date-difference datetime-difference)
       (attr e 'DTEND) (attr e 'DTSTART))))

(define-public (event-length/clamped start-date end-date e)
  (if (date? (attr e 'DTSTART))
      (date-difference (date-min (date+ end-date (date day: 1))
                                 (attr e 'DTEND))
                       (date-max start-date
                                 (attr e 'DTSTART)))
      (datetime-difference (datetime-min (datetime date: (date+ end-date (date day: 1)))
                                         (get-datetime (attr e 'DTEND)))
                           (datetime-max (datetime date: start-date)
                                         (get-datetime (attr e 'DTSTART))))))

;; Returns the length of the part of @var{e} which is within the day
;; starting at the time @var{start-of-day}.
;; currently the secund argument is a date, but should possibly be changed
;; to a datetime to allow for more explicit TZ handling?
(define-public (event-length/day date e)
  ;; TODO date= > 2 elements
  (cond [(and (date= (as-date (attr e 'DTSTART))
                 (as-date (attr e 'DTEND)))
              (date= (as-date (attr e 'DTSTART))
                     date))
         (time- (as-time (attr e 'DTEND))
                (as-time (attr e 'DTSTART)))]
        ;; Starts today, end in future day
        [(date= (as-date (attr e 'DTSTART))
                date)
         (time- #24:00:00 (as-time (attr e 'DTSTART)))]
        ;; Ends today, start earlier day
        [(date= (as-date (attr e 'DTEND))
                date)
         (as-time (attr e 'DTEND))]
        ;; start earlier date, end later date
        [else #24:00:00]))


;; 22:00 - 03:00
;; 2h för dag 1
;; 3h för dag 2

;; An event is considered long if it's DTSTART (and thereby DTEND) lacks a time component,
;; or if the total length of the event is greater than 24h.
;; For practical purposes, an event being long means that it shouldn't be rendered as a part
;; of a regular day.
(define-public (long-event? ev)
  (or (date? (attr ev 'DTSTART))
      (datetime<= (datetime date: (date day: 1))
                  (datetime-difference (attr ev 'DTEND)
                                       (attr ev 'DTSTART)))))





(define-public (zoneinfo->vtimezone zoneinfo zone-name)
  (define vtimezone (make-vcomponent 'VTIMEZONE))
  (define last-until (datetime date: #1000-01-01))
  (define last-offset (timespec-zero))
  (set! (attr vtimezone 'TZID) zone-name)
  (for zone-entry in (get-zone zoneinfo zone-name)
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
                        (drop-while
                         ;; TODO
                         ;; some of the rules might not apply to us since we only
                         ;; started using that rule set later. It's also possible
                         ;; that we stopped using a ruleset which continues existing.
                         ;;
                         ;; Both these cases should be filtered here.
                         (const #f)
                         #;
                         (lambda (rule)
                           (rule-from rule) ; 1970
                           (rule-in rule)   ; 4 (apr)
                           (rule-on rule)   ; 5 | (last sat)

                           (rule-to rule)

                           last-until)
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

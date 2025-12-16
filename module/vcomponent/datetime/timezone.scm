(define-module (vcomponent datetime timezone)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 match)
  :use-module (hnh util type)
  :use-module (hnh util exceptions)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (vcomponent)
  :use-module (vcomponent create)
  :use-module (vcomponent type recurrence)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (datetime zic)
  :export (zoneinfo->vtimezone))


;; Checks if the given zone-entry is relevant for this event
;; by checking if zone-entry-until isn't before our DTSTART.
(define ((relevant-zone-entry? start-dt) zone-entry)
  (typecheck start-dt datetime?)
  (typecheck zone-entry zone-entry?)

  (aif (zone-entry-until zone-entry)
       (datetime<? start-dt it)
       #t))

;;; Creates a predicate, which tests if a given zoneinfo rule
;;; overlapps with the given interval.
(define ((relevant-zone-rule? start-year end-year) rule)
  (typecheck start-year integer?)
  (typecheck end-year (or false? integer?))
  (typecheck rule zi-rule?)

  (match (list end-year (rule-to rule))
    [(#f 'only)    (= start-year (rule-from rule))]
    [(_  'only)    (<= start-year (rule-from rule) end-year)]
    [(_  'maximum) (<= (rule-from rule) start-year)]
    [(#f rule-to)  (<= (rule-from rule) start-year rule-to)]
    [(_  rule-to)  (or (<= start-year (rule-from rule) end-year)
                       (<= start-year rule-to end-year))]))

(define* (zoneinfo->vtimezone zoneinfo zone-name start-dt optional: end-year)
  (typecheck zoneinfo zoneinfo?)
  (typecheck zone-name string?)
  (typecheck start-dt datetime?)
  (typecheck end-year (or integer? false?))

  (define last-until (datetime date: (date month: 1 day: 1)))
  (define last-offset (timespec (time)))

  (fold (lambda (zone-entry vtimezone)
          (cond [(zone-entry-rule zone-entry) timespec?
                 => (lambda (inline-rule)
                      (let* ((new-timespec (timespec+
                                            (zone-entry-stdoff zone-entry)
                                            inline-rule))
                             (component
                              ;; TODO shouldn't this alternate between
                              ;; `daylight` and `standard`
                              (daylight
                               dtstart: last-until
                               tzoffsetfrom: last-offset
                               tzoffsetto: new-timespec
                               tzname: (zone-entry-format zone-entry))))
                        (set! last-until (zone-entry-until zone-entry)
                              last-offset new-timespec)
                        (add-child vtimezone component)))]

                [(zone-entry-rule zone-entry)
                 => (lambda (rule-name)
                      (fold (lambda (rule vtimezone)
                              (let* ((new-timespec (timespec+
                                                    (zone-entry-stdoff zone-entry)
                                                    (rule-save rule)))
                                     (component (create-vcomponent
                                                 ;; NOTE the zoneinfo database doesn't
                                                 ;; come with information if a given
                                                 ;; rule is in standard or daylight time,
                                                 ;; since that's mostly nonsencical
                                                 ;; (e.g. war- and peacetime).
                                                 ;; But the ical standard requires that,
                                                 ;; so this is a fair compromize.
                                                 (if (string-null? (rule-letters rule))
                                                     'STANDARD 'DAYLIGHT)

                                                 dtstart: (rule->dtstart rule)
                                                 tzoffsetfrom: last-offset
                                                 tzoffsetto: new-timespec
                                                 tzname: (zone-format (zone-entry-format zone-entry)
                                                                      (rule-letters rule)
                                                                      ;; TODO UTC offset, what do we actually want here?
                                                                      (timespec (time))))))

                                (set! ;; NOTE this can both be a number or the
                                    ;; symbol 'maximum
                                    last-until (zone-entry-until zone-entry)
                                    last-offset new-timespec)

                                (add-child
                                 vtimezone
                                 (cond ((rule->rrule rule)
                                        => (lambda (it) (set component (prop* 'RRULE) (just (list (vline value: it))))))
                                       (else component)))))
                            vtimezone
                            ;; some of the rules might not apply to us since we only
                            ;; started using that rule set later. It's also possible
                            ;; that we stopped using a ruleset which continues existing.
                            ;;
                            ;; Both these are filtered here.
                            (filter
                             (relevant-zone-rule? (year (datetime-date start-dt)) end-year)
                             (get-rule zoneinfo rule-name))))]

                [else (unreachable "zoneinfo->vtimezone" "" '())]))

        (-> (vcomponent type: 'VTIMEZONE)
            (set (prop* 'TZID) (just (list (vline value: zone-name)))))

        (filter (relevant-zone-entry? start-dt)
                (get-zone zoneinfo zone-name))))

(define-module (test add-and-save)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (datetime)
  ;; :use-module ((vcomponent) :select (prop))
  :use-module ((vcomponent base) :select (prop type children make-vcomponent))
  :use-module ((srfi srfi-1) :select (find))
  :use-module ((vcomponent formats vdir save-delete) :select (save-event))
  :use-module ((vcomponent formats xcal parse) :select (sxcal->vcomponent))
  :use-module ((vcomponent util instance methods)
               :select (add-calendars
                        add-and-save-event
                        remove-event
                        )))

;; TODO is this how I want to format direct components?

(define timezone
  '(vtimezone
    (properties (tzid (text "Europe/Stockholm")))
    (components
     (standard
      (properties
       (tzoffsetto (utc-offset "+0100"))
       (dtstart (date-time "1996-10-27T01:00:00"))
       (tzname (text "CET"))
       (tzoffsetfrom (utc-offset "+0200"))
       (rrule (recur (freq "YEARLY")
                     (interval "1")
                     ((byday "-1SU"))
                     ((bymonth 10))))))
     (daylight
      (properties
       (tzoffsetto (utc-offset "+0200"))
       (dtstart (date-time "1981-03-29T01:00:00"))
       (tzname (text "CEST"))
       (tzoffsetfrom (utc-offset "+0000"))
       (rrule (recur (freq "YEARLY")
                     (interval "1")
                     ((byday "-1SU"))
                     ((bymonth 3))))))))  )

(define ev
  (sxcal->vcomponent
   '(vevent
     (properties
      (uid (text "3da506ad-8d27-4810-94b3-6ab341baa1f2"))
      (summary (text "Test Event #1"))
      (dtstart
       (parameters (tzid (text "Europe/Stockholm")))
       (date-time "2021-12-21T10:30:00"))
      (dtstamp (date-time "2021-12-21T14:10:56Z"))
      (dtend (parameters (tzid (text "Europe/Stockholm")))
             (date-time "2021-12-21T11:45:00"))))))

(define rep-ev
  (sxcal->vcomponent
    '(vevent
      (properties
       (uid (text "4ebd6632-d192-4bf4-a33a-7a8388185914"))
       (summary (text "Repeating Test Event #1"))
       (rrule (recur (freq "DAILY")))
       (dtstart
        (parameters (tzid (text "Europe/Stockholm")))
        (date-time "2021-12-21T10:30:00"))
       (dtstamp (date-time "2021-12-21T14:10:56Z"))
       (dtend (parameters (tzid (text "Europe/Stockholm")))
              (date-time "2021-12-21T11:45:00"))))))

(define directory (tmpnam))

(define event-object ((@ (oop goops) make)
                      (@@ (vcomponent util instance methods) <events>)))

(mkdir directory)
(format #t "Using ~a~%" directory)

(define calendar (make-vcomponent 'VCALENDAR))

(set! (prop calendar '-X-HNH-SOURCETYPE) 'vdir
      (prop calendar '-X-HNH-DIRECTORY) directory)

(add-calendars event-object calendar)

;; Try adding   and saving a  new      regular   event
(add-and-save-event event-object calendar ev)

;; Try changing and saving an existing regular   event
(set! (prop ev 'SUMMARY) "Changed summary")
(add-and-save-event event-object calendar ev)

;; Try adding   and saving a  new      repeating event
(add-and-save-event event-object calendar rep-ev)

;; Try changing and saving an existing repeating event
;; TODO setting start time to later than end time leads to nonsense
;; errors when trying to generate the recurrence set.
(set! (prop rep-ev 'DTSTART) (datetime+ (prop rep-ev 'DTSTART)
                                        (datetime time: (time hour: 1))))
(add-and-save-event event-object calendar rep-ev)

;; Try adding   and saving a  new                event with multiple instances
;; Try changing and saving an existing           event with multiple instances

;; (add-and-save-event event-object calendar event)


(test-equal "Correct amount of children in calendar"
  2 (length (children calendar)))


(define get-events (@@ (vcomponent util instance methods) get-events))
(test-equal "Event object contains correct number of events (single calendar)"
  2 (length (get-events event-object)))

(remove-event event-object (car (get-events event-object)))

(test-equal "Correct number of events after removing first element"
  1 (length (get-events event-object)))

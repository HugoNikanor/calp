(define-module (test add-and-save)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (datetime timespec)
  ;; :use-module ((vcomponent) :select (prop))
  :use-module ((vcomponent base) :select (prop type children make-vcomponent))
  :use-module ((srfi srfi-1) :select (find))
  :use-module ((vcomponent formats vdir save-delete) :select (save-event))
  :use-module ((vcomponent create)
               :select (with-parameters
                        vcalendar vevent
                        vtimezone standard daylight))
  :use-module (vcomponent recurrence)
  :use-module ((vcomponent util instance methods)
               :select (add-calendars
                        add-and-save-event
                        remove-event
                        )))

(define timezone
  (vtimezone
   tzid: "Europe/Stockholm"
   (list
    (standard
     tzoffsetto: (parse-time-spec "01:00")
     dtstart: #1996-10-27T01:00:00
     tzname: "CET"
     tzoffsetfrom: (parse-time-spec "02:00")
     rrule: (make-recur-rule
             freq: 'YEARLY
             interval: 1
             byday: (list (cons -1 sun))
             bymonth: (list 10)
             ))
    (daylight
     tzoffsetto: (parse-time-spec "02:00")
     dtstart: #1981-03-29T01:00:00
     tzname: "CEST"
     tzoffsetfrom: (parse-time-spec "00:00")
     rrule: (make-recur-rule
             freq: 'YEARLY
             interval: 1
             byday: (list (cons -1 sun))
             bymonth: (list 3))))))

(define ev
  (vevent
   uid: "3da506ad-8d27-4810-94b3-6ab341baa1f2"
   summary: "Test Event #1"
   dtstart: (with-parameters
             tzid: "Europe/Stockholm"
             #2021-12-21T10:30:00)
   dtstamp: #2021-12-21T14:10:56Z
   dtend: (with-parameters
           tzid: "Europe/Stockholm"
           #2021-12-21T11:45:00)))

(define rep-ev
  (vevent
   uid: "4ebd6632-d192-4bf4-a33a-7a8388185914"
   summary: "Repeating Test Event #1"
   rrule: (make-recur-rule freq: 'DAILY)
   dtstart: (with-parameters
             tzid: "Europe/Stockholm"
             #2021-12-21T10:30:00)
   dtstamp: #2021-12-21T14:10:56Z
   dtend: (with-parameters
           tzid: "Europe/Stockholm"
           #2021-12-21T11:45:00)
   ))

;; TODO tmpnam is deprecated
(define directory (tmpnam))

(define event-object ((@ (oop goops) make)
                      (@@ (vcomponent util instance methods) <events>)))

(mkdir directory)
(format #t "Using ~a~%" directory)

(define calendar
  (vcalendar
   #:-X-HNH-SOURCETYPE 'vdir
   #:-X-HNH-DIRECTORY directory
   ))

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
  5 (length (children calendar)))


(define get-events (@@ (vcomponent util instance methods) get-events))
(test-equal "Event object contains correct number of events (single calendar)"
  2 (length (get-events event-object)))

(remove-event event-object (car (get-events event-object)))

(test-equal "Correct number of events after removing first element"
  1 (length (get-events event-object)))

(define-module (test vcomponent-equal)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (vcomponent create)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type utc-offset)
  :use-module ((vcomponent) :select (vcomponent-equal?))
  )

;;; Things of note:
;;; - the childrens are in different order
;;; - All properties are in a random order
;;; TODO Add parameters
(test-assert "vcomponent equal?"
    (vcomponent-equal?
     (vtimezone tzid: "Europe/Zurich"
                (list
                 (daylight
                  tzoffsetfrom: (utc-offset value: 0)
                  tzoffsetto: (utc-offset value: 7200)
                  tzname: "CEST"
                  dtstart: (datetime year: 1981 month: 3 day: 29 hour: 1 tz: "UTC")
                  uid: "d19c9347-9a85-4432-a876-5fb9c0d24d2b"
                  rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun))
                                     bymonth: '(3) wkst: monday))
                 (standard
                  tzoffsetfrom: (utc-offset value: 7200)
                  uid: "7dce30d4-6aaa-4cfb-85dc-813f74d7f4a9"
                  dtstart: (datetime year: 1996 month: 10 day: 27 hour: 1 tz: "UTC")
                  rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun))
                                     bymonth: '(10) wkst: monday)
                  tzoffsetto: (utc-offset value: 3600)
                  tzname: "CET")))

     (vtimezone tzid: "Europe/Zurich"
                (list
                 (standard
                  dtstart: (datetime year: 1996 month: 10 day: 27 hour: 1 tz: "UTC")
                  rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun))
                                     bymonth: '(10) wkst: monday)
                  tzname: "CET"
                  tzoffsetfrom: (utc-offset value: 7200)
                  tzoffsetto: (utc-offset value: 3600)
                  uid: "7dce30d4-6aaa-4cfb-85dc-813f74d7f4a9")
                 (daylight
                  dtstart: (datetime year: 1981 month: 3 day: 29 hour: 1 tz: "UTC")
                  rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun))
                                     bymonth: '(3) wkst: monday)
                  tzname: "CEST"
                  tzoffsetfrom: (utc-offset value: 0)
                  tzoffsetto: (utc-offset value: 7200)
                  uid: "d19c9347-9a85-4432-a876-5fb9c0d24d2b")))))


'((vcomponent))

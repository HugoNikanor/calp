;;; -*- mode: scheme -*-
(vcalendar
 #:prodid "-//Example Corp.//Example Client//EN"
 #:version (vcalendar-version #:min #f #:max "2.0")
 (list (vtimezone
        #:last-modified #2004-01-10T03:28:45Z
        #:tzid "US/Eastern"
        (list (daylight
               #:dtstart #2000-04-04T02:00:00
               #:rrule (recur-rule
                        #:freq 'YEARLY
                        #:interval 1
                        #:byday (list (cons 1 sun))
                        #:bymonth (list 4)
                        #:wkst mon)
               #:tzname "EDT"
               #:tzoffsetfrom (utc-offset #:value (* -5 3600))
               #:tzoffsetto (utc-offset #:value (* -4 3600)))
              (standard
               #:dtstart #2000-10-26T02:00:00
               #:rrule (recur-rule
                        #:freq 'YEARLY
                        #:interval 1
                        #:byday (list (cons -1 sun))
                        #:bymonth (list 10)
                        #:wkst mon)
               #:tzname "EST"
               #:tzoffsetfrom (utc-offset #:value (* -4 3600))
               #:tzoffsetto (utc-offset #:value (* -5 3600)))))
       (vevent
        #:description "We are having a meeting all this week at 12 pm for one hour, with an additional meeting on the first day 2 hours long.\nPlease bring your own lunch for the 12 pm meetings."
        #:dtstamp #2006-02-06T00:11:21Z
        #:dtstart (datetime #:date #2006-01-02 #:time #12:00:00 #:tz "US/Eastern")
        #:duration (duration #:sign '+ #:hour 1)
        #:rdate (period
                 #:start (datetime #:date #2006-01-02 #:time #15:00:00 #:tz "US/Eastern")
                 #:end (duration #:sign '+ #:hour 2))
        #:rrule (recur-rule #:freq 'DAILY #:count 5 #:interval 1 #:wkst 1)
        #:summary "Event #2"
        #:uid "00959BC664CA650E933C892C@example.com")
       (vevent
        #:dtstamp #2006-02-06T00:11:21Z
        #:dtstart (datetime #:date #2006-01-04 #:time #14:00:00 #:tz "US/Eastern")
        #:duration (duration #:sign '+ #:hour 1)
        #:recurrence-id (datetime #:date #2006-01-04 #:time #12:00:00 #:tz "US/Eastern")
        #:summary "Event #2 bis"
        #:uid "00959BC664CA650E933C892C@example.com")))

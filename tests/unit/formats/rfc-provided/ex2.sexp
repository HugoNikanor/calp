;;; -*- mode: scheme -*-
(vcomponent
 #:type
 'VCALENDAR
 #:properties
 (-> (table)
     (table-put 'PRODID (list (vline #:params (table) #:value "-//Example Corp.//Example Client//EN")))
     (table-put 'VERSION (list (vline #:params (table) #:value (vcalendar-version #:min #f #:max "2.0")))))
 #:children
 (list (vcomponent
        #:type
        'VTIMEZONE
        #:properties
        (-> (table)
            (table-put
             'LAST-MODIFIED
             (list (vline #:params (table) #:value (datetime #:date #2004-01-10 #:time #03:28:45 #:tz "UTC"))))
            (table-put 'TZID (list (vline #:params (table) #:value "US/Eastern"))))
        #:children
        (list (vcomponent
               #:type
               'DAYLIGHT
               #:properties
               (-> (table)
                   (table-put
                    'DTSTART
                    (list (vline #:params (table) #:value (datetime #:date #2000-04-04 #:time #02:00:00 #:tz #f))))
                   (table-put
                    'RRULE
                    (list (vline #:params
                                 (table)
                                 #:value
                                 (recur-rule #:freq 'YEARLY
                                             #:interval 1
                                             #:byday (list (cons 1 0))
                                             #:bymonth (list 4)
                                             #:wkst 1))))
                   (table-put 'TZNAME (list (vline #:params (table) #:value "EDT")))
                   (table-put
                    'TZOFFSETFROM
                    (list (vline #:params
                                 (table)
                                 #:value
                                 (timespec (time #:hour 5 #:minute 0 #:second 0) '- 'utc))))
                   (table-put
                    'TZOFFSETTO
                    (list (vline #:params
                                 (table)
                                 #:value
                                 (timespec (time #:hour 4 #:minute 0 #:second 0) '- 'utc)))))
               #:children
               (list))
              (vcomponent
               #:type
               'STANDARD
               #:properties
               (-> (table)
                   (table-put
                    'DTSTART
                    (list (vline #:params (table) #:value (datetime #:date #2000-10-26 #:time #02:00:00 #:tz #f))))
                   (table-put
                    'RRULE
                    (list (vline #:params
                                 (table)
                                 #:value
                                 (recur-rule #:freq 'YEARLY
                                             #:interval 1
                                             #:byday (list (cons -1 0))
                                             #:wkst 1
                                             #:bymonth (list 10)))))
                   (table-put 'TZNAME (list (vline #:params (table) #:value "EST")))
                   (table-put
                    'TZOFFSETFROM
                    (list (vline #:params
                                 (table)
                                 #:value
                                 (timespec (time #:hour 4 #:minute 0 #:second 0) '- 'utc))))
                   (table-put
                    'TZOFFSETTO
                    (list (vline #:params
                                 (table)
                                 #:value
                                 (timespec (time #:hour 5 #:minute 0 #:second 0) '- 'utc)))))
               #:children
               (list))))
       (vcomponent
        #:type
        'VEVENT
        #:properties
        (-> (table)
            (table-put
             'DESCRIPTION
             (list (vline #:params
                          (table)
                          #:value
                          "We are having a meeting all this week at 12 pm for one hour, with an additional meeting on the first day 2 hours long.\nPlease bring your own lunch for the 12 pm meetings.")))
            (table-put 'DTSTAMP (list (vline #:params (table) #:value (datetime #:date #2006-02-06 #:time #00:11:21 #:tz "UTC"))))
            (table-put
             'DTSTART
             (list (vline #:value (datetime #:date #2006-01-02 #:time #12:00:00 #:tz "US/Eastern"))))
            (table-put 'DURATION (list (vline #:params (table) #:value (duration #:sign '+ #:day #f #:time #01:00:00))))
            (table-put
             'RDATE
             (list (vline #:value
                          (period
                           #:start
                           (datetime #:date #2006-01-02 #:time #15:00:00 #:tz "US/Eastern")
                           #:end
                           (duration #:sign '+ #:day #f #:time #02:00:00)))))
            (table-put 'RRULE (list (vline #:params (table) #:value (recur-rule #:freq 'DAILY #:count 5 #:interval 1 #:wkst 1))))
            (table-put 'SUMMARY (list (vline #:params (table) #:value "Event #2")))
            (table-put 'UID (list (vline #:params (table) #:value "00959BC664CA650E933C892C@example.com"))))
        #:children
        (list))
       (vcomponent
        #:type
        'VEVENT
        #:properties
        (-> (table)
            (table-put 'DTSTAMP (list (vline #:params (table) #:value (datetime #:date #2006-02-06 #:time #00:11:21 #:tz "UTC"))))
            (table-put
             'DTSTART
             (list (vline #:value (datetime #:date #2006-01-04 #:time #14:00:00 #:tz "US/Eastern"))))
            (table-put 'DURATION (list (vline #:params (table) #:value (duration #:sign '+ #:day #f #:time #01:00:00))))
            (table-put
             'RECURRENCE-ID
             (list (vline #:value (datetime #:date #2006-01-04 #:time #12:00:00 #:tz "US/Eastern"))))
            (table-put 'SUMMARY (list (vline #:params (table) #:value "Event #2 bis")))
            (table-put 'UID (list (vline #:params (table) #:value "00959BC664CA650E933C892C@example.com"))))
        #:children
        (list))))

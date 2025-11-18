(vcomponent
 #:type
 'VCALENDAR
 #:properties
 (-> (table)
     (table-put 'CALSCALE (list (vline #:params (table) #:value "GREGORIAN")))
     (table-put 'PRODID (list (vline #:params (table) #:value "-//Example Inc.//Example Calendar//EN")))
     (table-put 'VERSION (list (vline #:params (table) #:value (vcalendar-version #:min #f #:max "2.0")))))
 #:children
 (list (vcomponent
        #:type
        'VEVENT
        #:properties
        (-> (table)
            (table-put
             'DTSTAMP
             (list (vline #:params (table)
                          #:value (datetime #:date #2008-02-05 #:time #19:12:24 #:tz "UTC"))))
            (table-put 'DTSTART (list (vline #:params (table) #:value (date #:year 2008 #:month 10 #:day 6))))
            (table-put 'SUMMARY (list (vline #:params (table) #:value "Planning meeting")))
            (table-put 'UID (list (vline #:params (table) #:value "4088E990AD89CB3DBB484909"))))
        #:children
        (list))))

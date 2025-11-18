(vcomponent
 #:type
 'VCALENDAR
 #:properties
 (-> (table)
     (table-put 'GEO (list (vline #:value (geo #:y 58.41086 #:x 15.62157))))
     (table-put
      'REQUEST-STATUS
      (list (vline #:value
                   (request-status
                    #:statcode (list 3 1)
                    #:statdesc "Invalid property value"
                    #:extdata "DTSTART:96-Apr-01"))))
     (table-put
      'VERSION
      (list (vline #:value (vcalendar-version #:min "2.0" #:max "3.0"))))
     (table-put
      'X-BINARY
      (list (vline #:value
                   #vu8(32 95 95 95 95 95 95 10 60 32 116 101 115 116 32 62 10 32 45 45 45 45 45 45 10 32 32 32 32 32 32 32 32 92 32 32 32 94 95 95 94 10 32 32 32 32 32 32 32 32 32 92 32 32 40 111 111 41 92 95 95 95 95 95 95 95 10 32 32 32 32 32 32 32 32 32 32 32 32 40 95 95 41 92 32 32 32 32 32 32 32 41 92 47 92 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 124 124 45 45 45 45 119 32 124 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 124 124 32 32 32 32 32 124 124 10))))
     (table-put
       'X-BOOLEAN
       (list (vline #:value #t #:params (-> (table) (table-put 'X-EXPECTED "true")))
             (vline #:value #f #:params (-> (table) (table-put 'X-EXPECTED "false")))))
     (table-put
      'X-CAL-ADDRESS
      (list (vline #:value (string->uri "mailto:hugo@example.com"))))
     (table-put
      'X-DATE
      (list (vline #:value (date #:year 2025 #:month 1 #:day 2))))
     (table-put
      'X-DATE-TIME
      (list (vline #:value
                   (datetime
                    #:date (date #:year 2025 #:month 1 #:day 2)
                    #:time (time #:hour 3 #:minute 4 #:second 5)
                    #:tz "Europe/Stockholm"))))
     (table-put
      'X-DURATION
      (list (vline #:value
                   (duration
                    #:sign '-
                    #:day 5
                    #:time (time #:hour 7 #:minute 0 #:second 0)))))
     (table-put 'X-FLOAT (list (vline #:value 3.141592653589793)))
     (table-put 'X-INTEGER (list (vline #:value 1729)))
     (table-put
      'X-PERIOD
      (list (vline #:value
                   (period
                     #:start (datetime
                               #:date (date #:year 2025 #:month 1 #:day 2)
                               #:time (time #:hour 3 #:minute 4 #:second 5)
                               #:tz #f)
                     #:end (duration #:sign '+ #:day 1 #:time #f)))))
     (table-put
       'X-RECUR
       (list (vline #:value
                    (recur-rule
                      #:freq 'MONTHLY
                      #:interval 1
                      #:byday (list (cons #f 1)
                                    (cons #f 2)
                                    (cons #f 3)
                                    (cons #f 4)
                                    (cons #f 5))
                      #:bysetpos (list -1)
                      #:wkst 1))))
     (table-put 'X-TEXT (list (vline #:value "This is some text")))
     (table-put
      'X-TIME
      (list (vline #:value (time #:hour 10 #:minute 20 #:second 30))))
     (table-put 'X-UNKNOWN (list (vline #:value "Handle, This!")))
     (table-put
      'X-URI
      (list (vline #:value (string->uri "https://example.com"))))
     (table-put
      'X-UTC-OFFSET
      (list (vline #:value
                   (timespec (time #:hour 2 #:minute 0 #:second 0) '+ 'utc)))))
 #:children
 (list (vcomponent
        #:type 'VEVENT
        #:properties
        (-> (table)
            (table-put
             'COMMENT
             (list (vline #:value
                          "Event part mostly exists to make this a \"compliant\" entry. Otherwise, this entry only exists to provide one instance of each known data-type as a property on the surrounding calendar object.\nGEO, VERSION, and REQUEST-STATUS are added to the calendars properties, since those have special handling, but no VALUE parameter to specify that such handling should occur. Likewise, X-UNKNOWN lacks a VALUE parameter, and should be parsed as an unknown (and therefore opaque) type.")))
            (table-put
              'DTSTAMP
              (list (vline #:value (datetime
                                     #:date (date #:year 2025 #:month 11 #:day 1)
                                     #:time (time #:hour 0 #:minute 0 #:second 0)
                                     #:tz "UTC"))))
            (table-put
              'DTSTART
              (list (vline #:value (datetime
                                     #:date (date #:year 2025 #:month 11 #:day 1)
                                     #:time (time #:hour 0 #:minute 0 #:second 0)
                                     #:tz "UTC"))))
            (table-put 'UID (list (vline #:value "sample-event"))))
        #:children
        (list))))

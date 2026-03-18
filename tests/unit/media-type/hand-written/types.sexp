;;; -*- mode: scheme -*-
(vcalendar
 #:geo (geo #:y 58.41086 #:x 15.62157)
 #:request-status (request-status
                   #:statcode (list 3 1)
                   #:statdesc "Invalid property value"
                   #:extdata "DTSTART:96-Apr-01")
 #:version (vcalendar-version #:min "2.0" #:max "3.0")
 #:x-binary #vu8(32 95 95 95 95 95 95 10 60 32 116 101 115 116 32 62 10 32 45 45 45 45 45 45 10 32 32 32 32 32 32 32 32 92 32 32 32 94 95 95 94 10 32 32 32 32 32 32 32 32 32 92 32 32 40 111 111 41 92 95 95 95 95 95 95 95 10 32 32 32 32 32 32 32 32 32 32 32 32 40 95 95 41 92 32 32 32 32 32 32 32 41 92 47 92 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 124 124 45 45 45 45 119 32 124 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 124 124 32 32 32 32 32 124 124 10)
 #:x-boolean (list (with-parameters #:x-expected "true"  #t)
                   (with-parameters #:x-expected "false" #f))
 #:x-cal-address (string->uri "mailto:hugo@example.com")
 #:x-date #2025-01-02
 #:x-date-time (tz #2025-01-02T03:04:05 "Europe/Stockholm")
 #:x-duration (duration #:sign '- #:day 5 #:hour 7)
 #:x-float 3.141592653589793
 #:x-integer 1729
 #:x-period (period
             #:start #2025-01-02T03:04:05
             #:end (duration #:sign '+ #:day 1))
 #:x-recur (recur-rule
            #:freq 'MONTHLY
            #:interval 1
            #:byday (list mon tue wed thu fri)
            #:bysetpos (list -1)
            #:wkst 1)
 #:x-text "This is some text"
 #:x-time #10:20:30
 #:x-unknown (unknown "Handle, This!")
 #:x-uri (string->uri "https://example.com")
 #:x-utc-offset (utc-offset #:value 7200)
 (list (vevent
        #:comment
        "Event part mostly exists to make this a \"compliant\" entry. Otherwise, this entry only exists to provide one instance of each known data-type as a property on the surrounding calendar object.\nGEO, VERSION, and REQUEST-STATUS are added to the calendars properties, since those have special handling, but no VALUE parameter to specify that such handling should occur. Likewise, X-UNKNOWN lacks a VALUE parameter, and should be parsed as an unknown (and therefore opaque) type."
        #:dtstamp #2025-11-01T00:00:00Z
        #:dtstart #2025-11-01T00:00:00Z
        #:uid "sample-event")))

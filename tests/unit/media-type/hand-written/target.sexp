(vcalendar
 #:calscale "GREGORIAN"
 #:prodid "-//CALP-TEST//x.y"
 #:request-status (request-status
  #:statcode (list 3 1)
  #:statdesc "Invalid property value"
  #:extdata "DTSTART:96-Apr-01")
 #:version (vcalendar-version #:min #f #:max "2.0")
 (list (vevent
        #:attach (with-parameters #:fmttype "text/plain" #vu8(10))
        #:class "PUBLIC"
        #:comment "A comment"
        #:completed (datetime #:date #2023-05-10 #:time #10:20:00 #:tz #f)
        #:description (list "Description of the event"
                            (with-parameters
                             #:language "sv"
                             "Beskrivning av händelsen"))
        #:dtstart (datetime #:date #2023-05-01 #:time #00:00:00 #:tz #f)
        #:location "Room 5"
        #:priority 5
        #:status "CANCELLED"
        #:summary "Event summary"
        #:uid "e4e812b8-dbb9-438d-ba56-ab58321fe4e1")))

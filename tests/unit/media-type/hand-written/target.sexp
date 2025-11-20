;;; -*- mode: scheme -*-
(vcomponent
 #:type
 'VCALENDAR
 #:properties
 (-> (table)
     (table-put 'CALSCALE (list (vline #:params (table) #:value "GREGORIAN")))
     (table-put 'PRODID (list (vline #:params (table) #:value "-//CALP-TEST//x.y")))
     (table-put 'REQUEST-STATUS
                (list (vline #:params (table)                        ;
                             #:value (request-status                 ;
                                      #:statcode (list 3 1)          ;
                                      #:statdesc "Invalid property value" ;
                                      #:extdata "DTSTART:96-Apr-01"))))
     (table-put 'VERSION (list (vline #:params (table) #:value (vcalendar-version #:min #f #:max "2.0")))))
 #:children
 (list (vcomponent
        #:type
        'VEVENT
        #:properties
        (-> (table)
            (table-put 'ATTACH (list (vline #:params (-> (table) (table-put 'FMTTYPE "text/plain")) #:value #vu8(10))))
            (table-put 'CLASS (list (vline #:params (table) #:value "PUBLIC")))
            (table-put 'COMMENT (list (vline #:params (table) #:value "A comment")))
            (table-put 'COMPLETED (list (vline #:params (table) #:value (datetime #:date #2023-05-10 #:time #10:20:00 #:tz #f))))
            (table-put
             'DESCRIPTION
             (list (vline #:params (table) #:value "Description of the event")
                   (vline #:params (-> (table) (table-put 'LANGUAGE "sv")) #:value "Beskrivning av händelsen")))
            (table-put 'DTSTART (list (vline #:params (table) #:value (datetime #:date #2023-05-01 #:time #00:00:00 #:tz #f))))
            (table-put 'LOCATION (list (vline #:params (table) #:value "Room 5")))
            (table-put 'PRIORITY (list (vline #:params (table) #:value 5)))
            (table-put 'STATUS (list (vline #:params (table) #:value "CANCELLED")))
            (table-put 'SUMMARY (list (vline #:params (table) #:value "Event summary")))
            (table-put 'UID (list (vline #:params (table) #:value "e4e812b8-dbb9-438d-ba56-ab58321fe4e1"))))
        #:children
        (list))))

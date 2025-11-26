;;; -*- mode: scheme -*-
(vcalendar
 #:version (vcalendar-version #:min #f #:max "2.0")
 (list (vevent #:summary (with-parameters #:x-param "Quoted; sure, thing!"
                                          "See parameter"))))

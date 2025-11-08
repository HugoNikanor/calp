;;;
;;; Simple configuration for the webdav entry point.
;;; 
;;; Gives an idea of how a CalDAV server could be configured, and is
;;; also used as the base for many (manual) tests.
;;; 

(use-modules (calp config-base)
             (scheme base)
             (hnh util path))


((@ (calp entry-points webdav) webdav-resources)
 `(virtual (; ("files" (file path: ,(getenv "HOME")))
            ; ("virtual" (virtual content: ,(string->utf8 "Test string\n")))
            ("explorer"
             (file
              path: ,(path-append (dirname (current-filename)) "webdav-explorer")))

            ("users"
             ;; TODO user list should be its own resource
             ;; type, instead of a hard coded list of users here.
             (virtual
              (("hugo"
                (principal
                 (("calendars"
                   (calendar-home
                    content-type: "text/plain"
                    content: ,(string->utf8 "This is the calendar home\n")
                    (("sqlite"
                      (calendar-collection "store:sqlite?path=/tmp/calendar.db"))
                     ("calendar.ics"
                      (calendar-collection "store:file?path=/tmp/calendar.ics&media=text/calendar"))
                     ("vdir"
                      (calendar-collection "store:vdir?path=/tmp/Calendar.vdir&media=text/calendar"))
                     ;; ("calendar.xcs"
                     ;;  (calendar-collection "store:file?path=/tmp/calendar.xcs&media=application/calendar+xml"))
                     ;; ("calendar.json"
                     ;;  (calendar-collection "store:file?path=/tmp/calendar.json&media=application/calendar+json"))
                     ))))))))))))

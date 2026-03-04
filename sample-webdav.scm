;;;
;;; Simple configuration for the webdav entry point.
;;; 
;;; Gives an idea of how a CalDAV server could be configured, and is
;;; also used as the base for many (manual) tests.
;;; 

(use-modules (calp config-base)
             (scheme base)
             (hnh util path)
             (srfi srfi-267))


(define root-content #"html"
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Hello!</title>
  </head>
  <body>
    <h1>Hello!</h1>
    <ul>
      <li><a href="/explorer/">WebDAV explorer of this domain</a>
      <li><a href="/users/">User list</a>
    </ul>
  </body>
</html>
  "html")

(define user-list-content #"html"
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>User list</title>
  </head>
  <body>
    <h1>User list</h1>
    <ul>
      <li><a href="hugo/">hugo</a>
    </ul>
  </body>
</html>
  "html")

((@ (calp entry-points webdav) webdav-resources)
 `(virtual
   content: ,(string->utf8 root-content)
   content-type: "text/html; charset=UTF-8"
   (
    ;; ("files" (file path: ,(getenv "HOME")))
    ;; ("virtual" (virtual content: ,(string->utf8 "Test string\n")))
    ("explorer" (file path: ,(path-append (dirname (current-filename)) "webdav-explorer")))

    ("users"
     ;; TODO user list should be its own resource
     ;; type, instead of a hard coded list of users here.
     (virtual
       content: ,(string->utf8 user-list-content)
       content-type: "text/html; charset=UTF-8"
       (("hugo"
         (calendar-home
          (
           ("rfc-sqlite" (calendar-collection "store://sqlite/tmp/rfc.db"))
           ("rfc-vdir" (calendar-collection "store://vdir/tmp/rfc-vdir?media=text/calendar"))
           ("rfc-rfc" (calendar-collection "store://webdav-report-xml/home/hugo/code/calp/tests/unit/rfc4791/webdav-report.xml"))
           )
          )))
       ;; (("hugo"
       ;;   (principal
       ;;     (("calendars"
       ;;       (calendar-home
       ;;         content-type: "text/plain"
       ;;         content: ,(string->utf8 "This is the calendar home\n")
       ;;         (("sqlite"
       ;;           (calendar-collection "store://sqlite/tmp/calendar.db"))
       ;;          ("calendar.ics"
       ;;           (calendar-collection "store://file/tmp/calendar.ics?media=text/calendar"))
       ;;          ("vdir"
       ;;           (calendar-collection "store://vdir/tmp/Calendar.vdir?media=text/calendar"))
       ;;          ;; ("calendar.xcs"
       ;;          ;;  (calendar-collection "store://file/tmp/calendar.xcs?media=application/calendar+xml"))
       ;;          ;; ("calendar.json"
       ;;          ;;  (calendar-collection "store://file/tmp/calendar.json?media=application/calendar+json"))
       ;;          )))))))
       )))))

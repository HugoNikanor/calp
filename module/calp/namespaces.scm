(define-module (calp namespaces))

;;; Commentary:
;;; (XML) Namespaces used by different parts of the program.
;;; Code:

(define-public webdav (string->symbol "DAV:"))
(define-public caldav (string->symbol "urn:ietf:params:xml:ns:caldav"))
(define-public xcal   (string->symbol "urn:ietf:params:xml:ns:icalendar-2.0"))
(define-public calp-namespace (string->symbol "http://hugo.hornquist.se/namespaces/calp"))

(define-public namespaces
  `((d . ,webdav)
    (c . ,caldav)
    (x . ,xcal)))

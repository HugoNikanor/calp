(define-module (ical)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (sort*))
  :use-module (hnh util path)
  :use-module ((rnrs io ports) :select (get-string-all))
  :use-module ((vcomponent formats ical) :prefix #{ics:}#)
  :export (sanitize-string
           serialize
           deserialize
           component-str))

;; Technically not back into source, since order of children isn't
;; stable. That's also why we just check that all lines are present,
;; regardless of order.
(define (sanitize-string str)
  (sort* (string-split str #\newline)
         string<))

(define serialize ics:serialize)
(define deserialize ics:deserialize)

(define component-str
  (call-with-input-file (path-append (getenv "here") "event.ics")
    get-string-all))

#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))
(use-modules (srfi srfi-19)
             (srfi srfi-26)
             (vcalendar))

(define cal (make-vcomponent "testcal/d1-b.ics"))

(for-each (cut transform-attr! <> "DTSTART"
               (cut string->date <> "~Y~m~dT~H~M~S"))
          (children cal))

(display (get-attr (car (children cal))
                   "DTSTART"))
(newline)
(display (get-attr (car (children cal))
                   "DTSTART"))
(newline)

#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-19)
             (srfi srfi-26)
             (vcalendar))

(define path
  (if (null? (cdr (command-line)))
      "testcal/d1-b.ics"
      (cadr (command-line))))

(define cal (make-vcomponent path))

(define (extract field)
  (cut get-attr <> field))

;;; Parse all start times into scheme date objects.
(for-each (cut transform-attr! <> "DTSTART"
               (cut string->date <> "~Y~m~dT~H~M~S"))
          (children cal  'VEVENT))

;;; This function borrowed from web-ics (calendar util) 
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

;;; Sort the events, and print a simple agenda.
(let ((sorted-events
       (sort* (children cal 'VEVENT)
              time<? (compose date->time-utc (extract "DTSTART")))))
  (for-each (lambda (ev) (format #t "~a | ~a~%"
                            (date->string (get-attr ev "DTSTART") "~1 ~H:~M")
                            (get-attr ev "SUMMARY")))
            sorted-events))


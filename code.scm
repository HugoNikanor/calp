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

;;; Parse all start times into scheme date objects.
(for-each (cut transform-attr! <> "DTSTART"
               (cut string->date <> "~Y~m~dT~H~M~S"))
          (children cal))

;;; Sort the events, and print a simple agenda.
(let ((sorted-events
       (sort (children cal)
             (lambda (a b)
               (time<? (date->time-utc (get-attr a "DTSTART"))
                       (date->time-utc (get-attr b "DTSTART")))))))
  (for-each (lambda (ev) (format #t "~a | ~a~%"
                            (date->string (get-attr ev "DTSTART") "~1 ~H:~M")
                            (get-attr ev "SUMMARY")))
            sorted-events))


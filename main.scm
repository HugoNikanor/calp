#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path ".")

(load "code.scm")
(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (vcalendar))

;;; ------------------------------------------------------------

(define (parse-dates! cal)
;;; Parse all start times into scheme date objects.
  (for-each (cut transform-attr! <> "DTSTART"
                 (lambda (start)
                   (localize-date
                    (string->date
                     start
                     (case (string-length start)
                       ((8) "~Y~m~d")
                       ((15) "~Y~m~dT~H~M~S")
                       ((16) "~Y~m~dT~H~M~S~z"))))))
            (children cal 'VEVENT)))

(define (search cal term)
  (cdr (let ((events (filter (lambda (ev) (eq? 'VEVENT (type ev)))
                             (children cal))))
         (find (lambda (ev) (string-contains-ci (car ev) term))
               (map cons (map (cut get-attr <> "SUMMARY")
                              events)
                    events)))))


(define (main args)
  (define path
    (if (null? (cdr (command-line)))
        "testcal/d1-b.ics"
        (cadr (command-line))))

  (define cal (make-vcomponent path))


  #; (define pizza-event (search cal "pizza"))

  (parse-dates! cal)

;;; Sort the events, and print a simple agenda.

  (let ((sorted-events
         (sort* (children cal 'VEVENT)
                time<? (compose date->time-utc (extract "DTSTART")))))
    (for-each (lambda (ev) (format #t "~a~a~a | ~a~%"
                              (if (date-today? (get-attr ev "DTSTART")) STR-YELLOW "")
                              (date->string (get-attr ev "DTSTART") "~1 ~H:~M")
                              STR-RESET
                              (get-attr ev "SUMMARY")))
              sorted-events))


  )

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
  (for-each-in (children cal 'VEVENT)
               (cut transform-attr! <> "DTSTART"
                    (lambda (start)
                      (localize-date
                        (string->date
                          start
                          (case (string-length start)
                            ((8) "~Y~m~d")
                            ((15) "~Y~m~dT~H~M~S")
                            ((16) "~Y~m~dT~H~M~S~z"))))))))

(define (search cal term)
  (cdr (let ((events (filter (lambda (ev) (eq? 'VEVENT (type ev)))
                             (children cal))))
         (find (lambda (ev) (string-contains-ci (car ev) term))
               (map-cons (cut get-attr <> "SUMMARY")
                         events)))))


(define (main args)
  (define path
    (if (null? (cdr (command-line)))
        "testcal/d1-b.ics"
        (cadr (command-line))))

  (define cal (make-vcomponent path))

  (parse-dates! cal)

  ;; Sort the events, and print a simple agenda.

  (for-each-in (sort* (children cal 'VEVENT)
                      time<? (compose date->time-utc (extract "DTSTART")))
               (lambda (ev) (format #t "~a | ~a~%"
                               (let ((start (get-attr ev "DTSTART")))
                                 (color-if (date-today? start) STR-YELLOW
                                           (date->string start "~1 ~H:~M")))
                               (get-attr ev "SUMMARY")))))


k
  #; (define pizza-event (search cal "pizza"))

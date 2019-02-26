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

(define (localize-date date)
  (time-utc->date (date->time-utc date)
                  (date-zone-offset (current-date))))

;;; Parse all start times into scheme date objects.
(for-each (cut transform-attr! <> "DTSTART"
               (lambda (start)
                 (localize-date (string->date start "~Y~m~dT~H~M~S~z"))))
          (children cal 'VEVENT))

;;; This function borrowed from web-ics (calendar util) 
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

(define STR-YELLOW "\x1b[0;33m")
(define STR-RESET "\x1b[m")

(define (date-today? input-date)
  (let* ((date (current-date))
         (now (make-date 0 0 0 0
                         (date-day date)
                         (date-month date)
                         (date-year date)
                         (date-zone-offset date)))
         (then (make-date 0 0 0 0
                          (1+ (date-day date))
                          (date-month date)
                          (date-year date)
                          (date-zone-offset date))))
    (and (time<=? (date->time-utc now)
                  (date->time-utc input-date))
         (time<=? (date->time-utc input-date)
                  (date->time-utc then)))))

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


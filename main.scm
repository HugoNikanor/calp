#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-19)
             (srfi srfi-19 util)
             (vcalendar)
             (vcalendar output)
             (util))

;;; This function borrowed from web-ics (calendar util) 
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

;;; ------------------------------------------------------------

(define (main args)
  (define path (cadr (append args '("testcal/repeating-event.ics"))))
  (define cal (make-vcomponent path))

  ;; Sort the events, and print a simple agenda.
  (for-each-in (sort* (children cal 'VEVENT)
                      time<? (extract "DTSTART"))
               (lambda (ev) (format #t "~a | ~a~%"
                               (let ((start (attr ev "DTSTART")))
                                 (color-if (today? start) STR-YELLOW
                                           (time->string start "~1 ~H:~M")))
                               (attr ev "SUMMARY")))))


  #; (define pizza-event (search cal "pizza"))

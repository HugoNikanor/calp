#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-26)
             (vcalendar)
             (util)
             (code))

;;; ------------------------------------------------------------


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
        "testcal/repeating-event.ics"
        (cadr (command-line))))

  (define cal (make-vcomponent path))

  ;; Sort the events, and print a simple agenda.

  (for-each-in (sort* (children cal 'VEVENT)
                      time<? (extract "DTSTART"))
               (lambda (ev) (format #t "~a | ~a~%"
                               (let ((start (get-attr ev "DTSTART")))
                                 (color-if (today? start) STR-YELLOW
                                           (time->string start "~1 ~H:~M")))
                               (get-attr ev "SUMMARY")))))


  #; (define pizza-event (search cal "pizza"))

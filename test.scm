#!/usr/bin/guile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs base)                ; assert
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-41)
             (code)
             (vcalendar)
             (vcalendar recur))

(define cal (make-vcomponent "testcal/repeating-event.ics"))

(define ev (find (lambda (ev) (eq? 'VEVENT (type ev)))
                 (children cal)))

(define ev-copy (copy-vcomponent ev))

(assert (equal? (children ev)
                (children ev-copy)))

(stream-for-each
 (lambda (ev)
   (format #t "~a -- ~a~%"
           (time->string (attr ev "DTSTART") "~1 ~3")
           (time->string (attr ev "DTEND") "~1 ~3")))
 (stream-take 10 (recur-event ev)))

(define stream-cadr (compose stream-car stream-cdr))

(newline)
(display (time->string (attr ev "DTSTART") "~1 ~3")) (newline)
(display (time->string (attr (stream-cadr (recur-event ev)) "DTSTART") "~1 ~3")) (newline)

#!/usr/bin/guile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs base)                ; assert
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-41)
             (code)
             (vcalendar)
             (vcalendar recur)
             (vcalendar datetime))

(define cal (make-vcomponent "testcal/repeating-event.ics"))

(define ev (find (lambda (ev) (eq? 'VEVENT (type ev)))
                 (children cal)))

(define ev-copy (copy-vcomponent ev))

(assert (equal? (children ev)
                (children ev-copy)))

(transform-attr! ev "DTSTART" parse-datetime)


(stream-for-each
 (lambda (ev)
   (display (date->string (attr ev "DTSTART") "~1 ~3")) (newline))
 (stream-take 10 (recur-event ev)))

(define stream-cadr (compose stream-car stream-cdr))

(newline)
(display (date->string (attr ev "DTSTART") "~1 ~3")) (newline)
(display (date->string (attr (stream-cadr (recur-event ev)) "DTSTART") "~1 ~3")) (newline)

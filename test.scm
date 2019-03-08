#!/usr/bin/guile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs base)                ; assert
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-41)
             (vcalendar)
             (vcalendar recur))

(define cal (make-vcomponent "testcal/repeating-event.ics"))

(define ev (car (children cal 'VEVENT)))

(define ev-copy (copy-vcomponent ev))

(assert (equal? (children ev)
                (children ev-copy)))

(define strm (recur-event ev))

(stream-for-each
 (lambda (ev)
   (format #t "~a -- ~a~%"
           (time->string (attr ev "DTSTART") "~1 ~3")
           (time->string (attr ev "DTEND") "~1 ~3")))
 (stream-take 10 (recur-event ev)))

(newline)

(for-each
 (lambda (ev)
   (format #t "~a -- ~a~%"
           (time->string (attr ev "DTSTART") "~1 ~3")
           (time->string (attr ev "DTEND") "~1 ~3")))
 (stream->list (stream-take 20 (recur-event ev))))

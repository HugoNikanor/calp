#!/usr/bin/guile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs base)                ; assert
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-41)
             (vcalendar)
             (vcalendar output)
             (vcalendar recur))

(define cal (make-vcomponent "testcal/repeating-event.ics"))

(define ev (car (children cal 'VEVENT)))

(define ev-copy (copy-vcomponent ev))

(assert (equal? (children ev)
                (children ev-copy)))

(define (display-timespan ev)
  (format #t "~a ~a ~a -- ~a~%"
          (attr ev 'NEW_ATTR)
          (attr ev 'N)
          (time->string (attr ev "DTSTART"))
          (time->string (attr ev "DTEND"))))

(display (attr ev 'N)) (newline)
(display-timespan ev)
(display (attr ev 'NEW_ATTR)) (newline)
(newline)
(define strm (generate-recurrence-set ev))
(display (attr ev 'RRULE)) (newline)

(if #f
    (begin
      (stream-for-each display-timespan (stream-take 20 strm))

      (newline)

      ;; (define strm (generate-recurrence-set ev))
      (display (attr ev 'RRULE)) (newline)

      ;; This makes the amount of events lookad at before have the same DTSTART,
      ;; which is the last from that set. The one's after that however are fine.
      (stream-for-each display-timespan (stream-take 40 strm))
      (newline)
      ;; This makes all the DTSTART be the last dtstart
      ;; (for-each display-timespan (stream->list (stream-take 20 strm)))

;;; I believe that I might have something to do with the stream's cache.

      (newline)

      (display-timespan ev)
      (display (attr ev 'NEW_ATTR))
      (newline))
    (begin
      ;; These two acts as one large unit.
      ;; Something modifies the initial ev even though it shouldn't
      (display-timespan ev)
      (stream-for-each
       display-timespan
       (stream-take 20 (generate-recurrence-set (copy-vcomponent ev))))
      (newline)
      (display-timespan ev)
      (newline)
      (stream-for-each
       display-timespan
       (stream-take 40 (generate-recurrence-set (copy-vcomponent ev))))
      (newline)
      (display-timespan ev)
      ))


#!/usr/bin/guile -s
!#

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/module"))

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-41)
             (srfi srfi-64)             ; Testisg

             (util)
             (vcalendar)
             (vcalendar output)
             (vcalendar recur))

(define (display-timespan ev)
  (format #t "~a -- ~a~%"
          (time->string (attr ev "DTSTART"))
          (time->string (attr ev "DTEND"))))

(define (tcal str)
  (format #f "~a/testdata/recurrence/~a"
          (dirname (dirname (current-filename)))
          str))

(test-begin "recurrence test")

(define cal-1 (make-vcomponent (tcal "simple-daily.ics")))

(let ((ev (car (children cal-1 'VEVENT))))
  (format #t "~a~%" (attr ev 'RRULE))

  (test-equal "Generate First"
    (map (extract 'DTSTART)
         (stream->list (stream-take 5 (generate-recurrence-set ev))))
    (let* ((s0 (attr ev 'DTSTART))
           (s1 (add-day s0))
           (s2 (add-day s1))
           (s3 (add-day s2))
           (s4 (add-day s3)))
      (list s0 s1 s2 s3 s4)))

  ;; We run the exact same thing a secound time, since I had an error with
  ;; that during development.
 (test-equal "Generate Again"
    (map (extract 'DTSTART)
         (stream->list (stream-take 5 (generate-recurrence-set ev))))
    (let* ((s0 (attr ev 'DTSTART))
           (s1 (add-day s0))
           (s2 (add-day s1))
           (s3 (add-day s2))
           (s4 (add-day s3)))
      (list s0 s1 s2 s3 s4))) )

(test-end "recurrence test")

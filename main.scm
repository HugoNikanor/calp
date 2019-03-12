#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-26)
             (ice-9 format)
             (util)
             (vcalendar)
             (vcalendar output)
             (terminal escape)
             (terminal util))

;;; ------------------------------------------------------------

#; (define pizza-event (search cal "pizza"))


(define-public (event-in? ev time)
  (in-day? (time-utc->date time)
           (attr ev 'DTSTART)))

(define (main-loop calendars)
  (define time (date->time-utc (current-date)))
  (let loop ((char #\nul))

    (case char
      ((#\L #\l) (set! time (add-day time)))
      ((#\h #\H) (set! time (remove-day time))))

    (cls)
    (display-calendar-header! (time-utc->date time))
    (line)

    (let ((events
           (sort* (concat
                   (map (lambda (cal)
                          (filter (cut event-in? <> time)
                                  (children cal 'VEVENT)))
                        calendars))
                  time<? (extract "DTSTART"))))

      (for-each-in events
       (lambda (ev)
         (format #t "~a~a | ~a | ~a~a~%"
                 (color-escape (attr (parent ev) 'COLOR))
                 (time->string (attr ev 'DTSTART) "~1 ~3") ; TODO show truncated string
                 (string-pad-right (attr ev 'SUMMARY) 30) ; TODO show truncated string
                 (or (attr ev 'LOCATION) "[INGEN LOKAL]")
                 STR-RESET))))

    ;; (format #t "c = ~c (~d)~%" char (char->integer char))

    (unless (or (eof-object? char)
                (memv char (list #\q (ctrl #\C))))
      (loop (read-char (current-input-port))))))

(load "config.scm")

(define (main args)

  (define calendars (map make-vcomponent calendar-files))

  (display calendar-files) (newline)

  (with-vulgar
   (lambda () (main-loop calendars))))



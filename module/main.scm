#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-26)
             (srfi srfi-41)
             (srfi srfi-41 util)
             (ice-9 format)
             (ice-9 control)            ; call-with-escape-continuation
             (texinfo string-utils)     ; string->wrapped-lines
             (util)
             (vcalendar)
             (vcalendar recur)
             (vcalendar datetime)
             (vcalendar output)
             (terminal escape)
             (terminal util))

(define (ev-time<? a b)
  (time<? (attr a 'DTSTART)
          (attr b 'DTSTART)))



#; (define pizza-event (search cal "pizza"))

(define (trim-to-width str len)
  (let ((trimmed (string-pad-right str len)))
    (if (< (string-length trimmed)
           (string-length str))
        (string-append (string-drop-right trimmed 1)
                       "…")
        trimmed)))
  ; TODO show truncated string

(define (main-loop regular-events repeating-events)
  (define time (date->time-utc (current-date)))
  (define cur-event 0)
  (call/ec
   (lambda (return)
    (let loop ((char #\nul))
      (let ((events
             ;; TODO change back to filter-sorted once it's fixed
             (merge (filter             ;-sorted
                     (cut event-in? <> time)
                     regular-events)

                    (stream->list
                     (filter-sorted-stream
                      (cut event-in? <> time)
                      repeating-events))

                    ev-time<?)))

        (case char
          ;; TODO The explicit loop call is a hack to rerender the display
          ;; It's REALLY ugly.
          ((#\L #\l) (set! time (add-day time))    (set! cur-event 0) (loop #\nul))
          ((#\h #\H) (set! time (remove-day time)) (set! cur-event 0) (loop #\nul))
          ((#\j #\J) (unless (= cur-event (1- (length events)))
                       (set! cur-event (1+ cur-event))))
          ((#\k #\K) (unless (= cur-event 0)
                       (set! cur-event (1- cur-event)))))

        (when (or (eof-object? char)
                  (memv char (list #\q (ctrl #\C))))
          (return #f))

        (cls)
        (display-calendar-header! (time-utc->date time))
        ;; (line)
        (format #t "~a┬~a┬~a~%"
                (make-string 20 #\─)
                (make-string 32 #\─)
                (make-string 10 #\─))


        (for-each
         (lambda (ev i)
           (format #t "~a │ ~a~a~a~a │ ~a~a~%"
                   (time->string (attr ev 'DTSTART) "~1 ~3") ; TODO show truncated string
                   (if (= i cur-event) "\x1b[7m" "")
                   (color-escape (attr (parent ev) 'COLOR))
                   (trim-to-width (attr ev 'SUMMARY) 30)
                   STR-RESET
                   (trim-to-width
                    (or (attr ev 'LOCATION) "\x1b[1;30mINGEN LOKAL") 20)
                   STR-RESET))
         events
         (iota (length events)))

        (format #t "~a┴~a┴~a~%"
                (make-string 20 #\─)
                (make-string 32 #\─)
                (make-string 10 #\─))

        (unless (null? events)
          (let ((ev (list-ref events cur-event)))
            (format #t "~a~%~a~%~aStart: ~a	Slut: ~a~%~%~a~%"
                    (attr ev 'X-HNH-FILENAME)
                    (attr ev 'SUMMARY)
                    (or (and=> (attr ev 'LOCATION) (cut string-append "Plats: " <> "\n")) "")
                    (time->string (attr ev 'DTSTART) "~1 ~3")
                    (time->string (attr ev 'DTEND) "~1 ~3")
                    (string-join   ; TODO replace this with a better text flower
                     (take-to      ; This one destroys newlines used for layout
                      (string->wrapped-lines (or (attr ev 'DESCRIPTION) "")
                                             #:line-width 60
                                             #:collapse-whitespace? #f)
                      10)
                     (string #\newline))
                    )))

        ;; (format #t "c = ~c (~d)~%" char (char->integer char))
        (loop (read-char (current-input-port)))
        )))))




(load "config.scm")

(define (main args)

  (define calendars (map make-vcomponent calendar-files))
  (define events (concatenate (map (cut children <> 'VEVENT) calendars)))

  (let* ((repeating regular (partition repeating? events)))

    (set! repeating (sort*! repeating time<? (extract 'DTSTART)))
    (set! regular (sort*! regular   time<? (extract 'DTSTART)))

    (let ((repeating (interleave-streams ev-time<?
                      (map generate-recurrence-set repeating))))
      (with-vulgar
       (lambda () (main-loop regular repeating))))))



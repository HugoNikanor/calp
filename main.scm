#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-26)
             (ice-9 format)
             (texinfo string-utils)     ; string->wrapped-lines
             (util)
             (vcalendar)
             (vcalendar output)
             (terminal escape)
             (terminal util))

(define (take-to lst i)
  (if (> i (length lst))
      lst (take lst i)))


;;; ------------------------------------------------------------

#; (define pizza-event (search cal "pizza"))

;;    A          B          C          D         ¬E
;; |s1|     :     |s2| : |s1|     :     |s2| : |s1|
;; |  |     :     |  | : |  ||s2| : |s1||  | : |  |
;; |  ||s2| : |s1||  | : |  ||  | : |  ||  | :
;;     |  | : |  |     : |  ||  | : |  ||  | :     |s2|
;;     |  | : |  |     : |  |     :     |  | :     |  |
(define (timespan-overlaps? s1-begin s1-end s2-begin s2-end)
  "Return whetever or not two timespans overlap."
  (or
   ;; A
   (and (time<=? s2-begin s1-end)
        (time<=? s1-begin s2-end))

   ;; B
   (and (time<=? s1-begin s2-end)
        (time<=? s2-begin s1-end))

   ;; C
   (and (time<=? s1-begin s2-begin)
        (time<=? s2-end s1-end))

   ;; D
   (and (time<=? s2-begin s1-begin)
        (time<=? s1-end s2-end))))

(define (event-overlaps? event begin end)
  "Returns if the event overlaps the timespan.
Event must have the DTSTART and DTEND attribute set."
  (timespan-overlaps? (attr event 'DTSTART)
                      (attr event 'DTEND)
                      begin end))

(define-public (event-in? ev time)
  (let* ((date (time-utc->date time))
         (start (date->time-utc (drop-time date)))
         (end (add-duration start (make-duration (* 60 60 24)))))
    (event-overlaps? ev start end)))

(define (trim-to-width str len)
  (let ((trimmed (string-pad-right str len)))
    (if (< (string-length trimmed)
           (string-length str))
        (string-append (string-drop-right trimmed 1)
                       "…")
        trimmed)))
  ; TODO show truncated string

(define (main-loop calendars)
  (define time (date->time-utc (current-date)))
  (define cur-event 0)
  (let loop ((char #\nul))
    (let ((events
           (sort* (concat
                   (map (lambda (cal)
                          (filter (cut event-in? <> time)
                                  (children cal 'VEVENT)))
                        calendars))
                  time<? (extract "DTSTART"))))

      (case char
        ((#\L #\l) (set! time (add-day time)) (set! cur-event 0))
        ((#\h #\H) (set! time (remove-day time)) (set! cur-event 0))
        ((#\j #\J) (unless (= cur-event (1- (length events)))
                     (set! cur-event (1+ cur-event))))
        ((#\k #\K) (unless (= cur-event 0)
                     (set! cur-event (1- cur-event)))))

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

      (let ((ev (list-ref events cur-event)))
        (format #t "~a~%~aStart: ~a	Slut: ~a~%~%~a~%"
                (attr ev 'SUMMARY)
                (or (and=> (attr ev 'LOCATION) (cut string-append "Plats: " <> "\n")) "")
                (time->string (attr ev 'DTSTART) "~1 ~3")
                (time->string (attr ev 'DTEND) "~1 ~3")
                (string-join ; TODO replace this with a better text flower
                 (take-to ; This one destroys newlines used for layout
                  (string->wrapped-lines (or (attr ev 'DESCRIPTION) "")
                                         #:line-width 60
                                         #:collapse-whitespace? #f)
                  10)
                 (string #\newline))
                ))

      ;; (format #t "c = ~c (~d)~%" char (char->integer char))

      (unless (or (eof-object? char)
                  (memv char (list #\q (ctrl #\C))))
        (loop (read-char (current-input-port)))))))

(load "config.scm")

(define (main args)

  (define calendars (map make-vcomponent calendar-files))

  (display calendar-files) (newline)

  (with-vulgar
   (lambda () (main-loop calendars))))



(define-module (output terminal)
  #:use-module (output general)
  #:use-module (output text)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (util)
  #:use-module (terminal escape)
  #:use-module (terminal util)
  #:use-module (vcomponent output)
  #:use-module (vcomponent group)

  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)

  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (parameters)
  #:use-module (config)

  #:export (terminal-main))

(define (box-top intersection line . lengths)
  (reduce (lambda (str done) (string-append done (string intersection) str))
          "" (map (cut make-string <> line) lengths)))

(define* (display-event-table events #:key
                              (cur-event -1)
                              (summary-width 30)
                              (location-width 20))
 (for-each
  (lambda (ev i)
    (format #t "~a │ ~a~a~a~a │ ~a~a~a~%"
            (time->string (attr ev 'DTSTART) "~1 ~3") ; TODO show truncated string
            (if (= i cur-event) "\x1b[7m" "")
            (color-escape (attr (parent ev) 'COLOR))
            ;; Summary filter is a hook for the user
            (trim-to-width ((summary-filter) ev (attr ev 'SUMMARY)) summary-width)
            STR-RESET
            (if (attr ev 'LOCATION) "" "\x1b[1;30m")
            (trim-to-width
             (or (attr ev 'LOCATION) "INGEN LOKAL") location-width)
            STR-RESET))
  events
  (iota (length events))))

(define (displayln a)
  (display a) (newline))

(define (main-loop time event-stream)
  (define cur-event 0)

  (define-values (height width) (get-terminal-size))

  (while #t
    (let ((events
           (group->event-list
            (stream-car
             ;; TODO reusing the same grouping causes it to lose events.
             ;; I currently have no idea why, but it's BAD.
             (get-groups-between (group-stream event-stream)
                                 (time-utc->date time) (time-utc->date time))))))

      (cls)
      (display-calendar-header! (time-utc->date time))

      (let* ((date-width 20)
             (location-width 15)
             (summary-width (- width date-width location-width 6)))
        (displayln
         (box-top #\┬ #\─ date-width (+ summary-width 2) (1+ location-width)))
        (display-event-table
         events
         #:cur-event cur-event
         #:location-width location-width
         #:summary-width summary-width)
        (displayln
         (box-top #\┴ #\─ date-width (+ summary-width 2) (1+ location-width))))

      (unless (null? events)
        (let ((ev (list-ref events cur-event)))
          (format #t "~a~%~%  ~a~%~%~a\x1b[1mStart:\x1b[m ~a	\x1b[1mSlut:\x1b[m ~a~%~%~a~%"
                  (attr ev 'X-HNH-FILENAME)
                  (attr ev 'SUMMARY)
                  (or (and=> (attr ev 'LOCATION)
                             (cut string-append "\x1b[1mPlats:\x1b[m " <> "\n")) "")
                  (time->string (attr ev 'DTSTART) "~1 ~3")
                  (time->string (attr ev 'DTEND) "~1 ~3")
                  (unlines (take-to (flow-text (or (attr ev 'DESCRIPTION) "")
                                               #:width (min 70 width))
                                    (- height 8 5 (length events) 5))))))

      (let ((char (read-char)))
        ;; (format (current-error-port)
        ;;         "c = ~c (~d)~%" char (char->integer char))
        (case char
          ((#\L #\l)
           (set! time (add-day time)
                 cur-event 0))
          ((#\h #\H)
           (set! time (remove-day time)
                 cur-event 0))
          ((#\t)
           (set! time (date->time-utc (drop-time (current-date)))
                 cur-event 0))
          ((#\j #\J) (unless (= cur-event (1- (length events)))
		       (mod! cur-event 1+)))
          ((#\k #\K) (unless (= cur-event 0)
		       (mod! cur-event 1-)))
          ((#\p) (print-vcomponent (list-ref events cur-event)
                                   (current-error-port)))
	  ((#\g) (set! cur-event 0))
	  ((#\G) (set! cur-event (1- (length events)))))

        (when (or (eof-object? char)
                  (memv char (list #\q (ctrl #\C))))
          (break)))
      )))

(define options
  '((date (value #t) (single-char #\d))))

(define (terminal-main calendars events args)
  (let ((opts (getopt-long args options)))
    (let ((time (date->time-utc
                 (drop-time (or (and=> (option-ref opts 'date #f) parse-freeform-date)
                                (current-date))))))
      (with-vulgar
       (lambda () (main-loop time events))))))

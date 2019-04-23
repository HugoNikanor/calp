(define-module (output terminal)
  #:use-module (output general)
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

  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)

  #:use-module (texinfo string-utils)     ; string->wrapped-lines
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (parameters)
  #:use-module (config)

  #:export (terminal-main))

(define (box-top intersection line . lengths)
  (reduce (lambda (str done) (string-append done (string intersection) str))
          "" (map (cut make-string <> line) lengths)))

(define (display-event-table cur-event events)
 (for-each
  (lambda (ev i)
    (format #t "~a │ ~a~a~a~a │ ~a~a~%"
            (time->string (attr ev 'DTSTART) "~1 ~3") ; TODO show truncated string
            (if (= i cur-event) "\x1b[7m" "")
            (color-escape (attr (parent ev) 'COLOR))
            ;; Summary filter is a hook for the user
            (trim-to-width ((summary-filter) ev (attr ev 'SUMMARY)) 30)
            STR-RESET
            (trim-to-width
             (or (attr ev 'LOCATION) "\x1b[1;30mINGEN LOKAL") 20)
            STR-RESET))
  events
  (iota (length events))))

(define (displayln a)
  (display a) (newline))

(define (main-loop time event-stream)
  (define cur-event 0)


  (while #t
    (let ((events
           (stream->list
            (filter-sorted-stream
             (cut event-in? <> time)
             event-stream))))

      (cls)
      (display-calendar-header! (time-utc->date time))

      (displayln (box-top #\┬ #\─ 20 32 10))
      (display-event-table cur-event events)
      (displayln (box-top #\┴ #\─ 20 32 10))

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
           (set! time (date->time-utc (current-date))
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

(define (parse-freeform-date str)
  (string->date str "~Y-~m-~d"))

(define (terminal-main calendars events args)
  (let ((opts (getopt-long args options)))
    (let ((time (date->time-utc
                 (or (and=> (option-ref opts 'date #f) parse-freeform-date)
                     (current-date)))))
      (with-vulgar
       (lambda () (main-loop time events))))))

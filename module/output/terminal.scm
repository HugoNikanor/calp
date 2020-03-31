(define-module (output terminal)
  #:use-module (output general)
  #:use-module (output text)
  #:use-module (srfi srfi-1)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (util)
  #:use-module (vulgar)
  #:use-module (vulgar info)
  #:use-module (vulgar color)
  #:use-module (vulgar components)
  #:use-module (vcomponent output)
  #:use-module (vcomponent group)

  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)

  #:use-module (ice-9 format)
  ;; #:use-module (parameters)

  #:export (main-loop))


(define (open-in-editor fname)
  (system (string-append (getenv "EDITOR") " " fname)))


(define (box-top intersection line . lengths)
  (reduce (lambda (str done) (string-append done (string intersection) str))
          "" (map (cut make-string <> line) lengths)))

(define* (display-event-table events #:key
                              (cur-event -1)
                              (summary-width 30)
                              (location-width 20))
 (for-each
  (lambda (ev i)
    (display
     (string-append
      (if (datetime? (attr ev 'DTSTART))
          (datetime->string (attr ev 'DTSTART) "~Y-~m-~d ~H:~M:~S")
          ((@ (texinfo string-utils) center-string)
           (date->string (attr ev 'DTSTART))
           19))
       ; TODO show truncated string
      " │ "
      (if (= i cur-event) "\x1b[7m" "")
      (color-escape (attr (parent ev) 'COLOR))
      ;; Summary filter is a hook for the user
      (let ((dirty (attr ev 'X-HNH-DIRTY)))
        (string-append
         (if dirty "* " "")
         (trim-to-width ((summary-filter) ev (attr ev 'SUMMARY)) (- summary-width
                                                                    (if dirty 2 0)))))
      STR-RESET
      " │ "
      (if (attr ev 'LOCATION) "" "\x1b[1;30m")
      (trim-to-width
       (or (attr ev 'LOCATION) "INGEN LOKAL") location-width)
      STR-RESET
      "\n")))
  events
  (iota (length events))))

(define (displayln a)
  (display a) (newline))

(define (main-loop date event-stream)
  (define cur-event 0)

  (define-values (height width) (get-terminal-size))

  (define grouped-stream (group-stream event-stream))

  (while #t
    ;; TODO reusing the same grouping causes it to lose events.
    ;; I currently have no idea why, but it's BAD.
    (let ((groups (get-groups-between grouped-stream
                                      date date)))
      (format (current-error-port) "len(groups) = ~a~%" (stream-length groups))
      (let ((events
             (if (stream-null? groups)
                 '() (group->event-list (stream-car groups)))))

        (cls)
        (display-calendar-header! date)

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
                    ;; NOTE RFC 5545 says that DTSTART and DTEND MUST
                    ;; have the same type. However we believe that is
                    ;; another story.
                    (let ((start (attr ev 'DTSTART)))
                      (if (datetime? start)
                          (datetime->string (attr ev 'DTSTART) "~Y-~m-~d ~H:~M:~S")
                          (date->string start)))
                    (let ((end (attr ev 'DTEND)))
                      (if (datetime? end)
                          (datetime->string (attr ev 'DTSTART) "~Y-~m-~d ~H:~M:~S")
                          (date->string end)))
                    (unlines (take-to (flow-text (or (attr ev 'DESCRIPTION) "")
                                                 #:width (min 70 width))
                                      (- height 8 5 (length events) 5))))))

        (let ((char (read-char)))
          ;; (format (current-error-port)
          ;;         "c = ~c (~d)~%" char (char->integer char))
          (case char
            ((#\L #\l)
             (set! date (add-day date)
                   cur-event 0))
            ((#\h #\H)
             (set! date (remove-day date)
                   cur-event 0))
            ((#\t)
             (set! date (current-date)
                   cur-event 0))
            ((#\j #\J) (unless (= cur-event (1- (length events)))
                         (mod! cur-event 1+)))
            ((#\k #\K) (unless (= cur-event 0)
                         (mod! cur-event 1-)))
            ((#\p) (print-vcomponent (list-ref events cur-event)
                                     (current-error-port)))
            ((#\E) (serialize-vcomponent (list-ref events cur-event)
                                         (open-output-file "/tmp/event.ics")))
            ((#\e)
             (let ((fname (tmpnam)))
               (with-output-to-file fname
                 (lambda () (serialize-vcomponent (list-ref events cur-event))))
               (open-in-editor fname)
               (let ((ev (parse-cal-path fname)))
                 (serialize-vcomponent ev (current-error-port))

                 ;; TODO remove child

                 (add-child! (parent (list-ref events cur-event)) ev)
                 (format (current-error-port) "Children: ~a~%start: ~a~%" (children ev)
                         (attr ev 'DTSTART))
                 (set! (attr ev 'X-HNH-DIRTY) #t)
                 (set! event-stream (stream-insert ev-time<? ev event-stream))
                 (set! grouped-stream (group-stream event-stream))
                 )))

            ((#\g) (set! cur-event 0))
            ((#\G) (set! cur-event (1- (length events)))))

          (when (or (eof-object? char)
                    (memv char '(#\q)))
            (break)))
        ))))

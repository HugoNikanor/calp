(define-module (output terminal)
  #:use-module (output general)
  #:use-module (srfi srfi-1)
  #:use-module (datetime)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (util)
  #:use-module ((util app) :prefix app/)
  #:use-module (vulgar)
  #:use-module (vulgar info)
  #:use-module (vulgar color)
  #:use-module (vulgar components)
  #:use-module (vcomponent group)

  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)

  #:use-module (text util)
  #:use-module (text flow)

  #:use-module (ice-9 format)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 sandbox)
  #:use-module (ice-9 match)

  #:use-module (vulgar termios)

  #:use-module (oop goops)
  #:use-module (oop goops describe)

  #:export (main-loop))

(define-values (height width) (get-terminal-size))

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
       (if (datetime? (prop ev 'DTSTART))
           (datetime->string (prop ev 'DTSTART) "~Y-~m-~d ~H:~M:~S")
           ((@ (texinfo string-utils) center-string)
            (date->string (prop ev 'DTSTART))
            19))
       " │ "
       (if (= i cur-event) "\x1b[7m" "")
       (color-escape (prop (parent ev) 'COLOR))
       ;; Summary filter is a hook for the user
       (let ((dirty (prop ev '-X-HNH-DIRTY)))
         (string-append
          (if dirty "* " "")
          ;; TODO reintroduce summary-filter
          (trim-to-width (prop ev 'SUMMARY) (- summary-width
                                               (if dirty 2 0)))))
       STR-RESET
       " │ "
       (if (prop ev 'LOCATION) "" "\x1b[1;30m")
       (trim-to-width
        (or (prop ev 'LOCATION) "INGEN LOKAL") location-width)
       STR-RESET
       "\n")))
   events
   (iota (length events))))

(define (displayln a)
  (display a) (newline))


(define-class <day-view> ()
  (event-set getter: get-event-set
             init-keyword: event-set:)
  (date accessor: view-date
        init-keyword: date:)
  (cur-event accessor: cur-event
             init-value: 0))


(define-method (output (this <day-view>))
  (define date (view-date this))
  (define event-set (get-event-set this))
  (define groups (group-stream event-set))
  (define group (get-groups-between groups date date))
  (define events (group->event-list (stream-car group)))

  (cls)

  (display "== Day View ==\n")

  (display-calendar-header! date)

  ;; display event list
  (let* ((date-width 20)
         (location-width 15)
         (summary-width (- width date-width location-width 6)))
    (displayln
     (box-top #\┬ #\─ date-width (+ summary-width 2) (1+ location-width)))
    (display-event-table
     events
     #:cur-event (cur-event this)
     #:location-width location-width
     #:summary-width summary-width)
    (displayln
     (box-top #\┴ #\─ date-width (+ summary-width 2) (1+ location-width))))

  ;; display highlighted event
  (unless (null? events)
    (let ((ev (list-ref events (cur-event this))))
      (format #t "~a~%~%  ~a~%~%~a\x1b[1mStart:\x1b[m ~a	\x1b[1mSlut:\x1b[m ~a~%~%~a~%"
              (prop ev '-X-HNH-FILENAME)
              (prop ev 'SUMMARY)
              (or (and=> (prop ev 'LOCATION)
                         (cut string-append "\x1b[1mPlats:\x1b[m " <> "\n")) "")
              ;; NOTE RFC 5545 says that DTSTART and DTEND MUST
              ;; have the same type. However we believe that is
              ;; another story.
              (let ((start (prop ev 'DTSTART)))
                (if (datetime? start)
                    (datetime->string (prop ev 'DTSTART) "~Y-~m-~d ~H:~M:~S")
                    (date->string start)))
              (let ((end (prop ev 'DTEND)))
                (if (datetime? end)
                    (datetime->string (prop ev 'DTEND) "~Y-~m-~d ~H:~M:~S")
                    (date->string end)))
              (unlines (take-to (flow-text (or (prop ev 'DESCRIPTION) "")
                                           #:width (min 70 width))
                                (- height 8 5 (length events) 5)))))))

(define-method (input (this <day-view>) char)
  (define date (view-date this))
  (define event-set (get-event-set this))
  (define groups (group-stream event-set))
  (define group (get-groups-between groups date date))
  (define events (group->event-list (stream-car group)))

  (case char
    ((#\L #\l)
     (set! (view-date this) (add-day date)
           (cur-event this) 0))
    ((#\h #\H)
     (set! (view-date this) (remove-day date)
           (cur-event this) 0))
    ((#\t)
     ;; TODO this should be local time
     ;; currently it's UTC (maybe?)
     (set! (view-date this) (current-date)
           (cur-event this) 0))
    ((#\j #\J) (unless (= (cur-event this) (1- (length events)))
                 (set! (cur-event this) = (+ 1))))
    ((#\k #\K) (unless (= (cur-event this) 0)
                 (set! (cur-event this) = (- 1))))
    ((#\g) (set! (cur-event this) 0))
    ((#\G) (set! (cur-event this) (1- (length events))))
    ((#\q) '(pop))
    ((#\() (set-cursor-pos 0 (1- height))
     (let* ((attr (make-termios))
            (search-term #f))
       (tcgetattr! attr)
       (set! (lflag attr) (logior ECHO (lflag attr)))
       (tcsetattr! attr)
       (set! search-term (readline "search: "))
       (set! (lflag attr) (logand (lognot ECHO) (lflag attr)))
       (tcsetattr! attr)
       `(push ,(search-view search-term (event-set this)))
       ))))

(define (day-view event-set date)
  (make <day-view> event-set: event-set date: date))

(define-class <search-view> ()
  (event-set getter: event-set init-keyword: event-set)
  (search-result getter: search-result)
  (search-term getter: search-term
               init-keyword: search-term:))

(define (search-view search-term event-set)
  (make <search-view> search-term: search-term event-set: event-set))

(define (prepare-string str)

  (define missing-parenthesis-count
   (string-fold (lambda (char count)
                  (case char
                    ((#\() (1+ count))
                    ((#\)) (1- count))
                    (else count)))
                0 str))

  (string-append str (make-string missing-parenthesis-count #\))))

(define-method (initialize (this <search-view>) args)
  (next-method)
  ;; (display (search-term this)) (newline)
  (slot-set! this 'search-result
             (stream-paginate
              (stream-filter
               (eval (print-and-return
                      `(lambda (event) ,(call-with-input-string
                                       (prepare-string (search-term this))
                                     read)))
                     (make-sandbox-module
                      `(
                        ((vcomponent base) prop)
                        ((ice-9 regex) string-match)
                        #;
                        ((datetime) ,@(module-map (lambda (a . _) a) ; ;
                        (resolve-module '(datetime))))
                        ,@all-pure-bindings)
                      ))
               (event-set this))))
  ;; (define current-page 0)
  ;; (define current-entry 0)
  )

(define-method (output (this <search-view>))
  (define page
    (catch #t
      (lambda ()
       (call-with-time-limit
        1
        (lambda () (stream->list (stream-ref (search-result this) 0)))
        (lambda _ (throw 'timed-out))))
      (lambda (err . args)
        (display (cons err args) (current-error-port))
        (newline  (current-error-port))
        'timed-out ; when search took to long
        'unbound-variable ; when search term has unbound variables
        '()

        )
      ))

  (cls)

  (display "== Search View ==\n")
  (for entry in page
       (let ((start (prop entry 'DTSTART)))
         (display
          (if (date? start)
              (date->string start "~Y-~m-~d ")
              (datetime->string start "~Y-~m-~d ~H:~M "))))
       (display (prop entry 'SUMMARY))
       (newline)))

(define-method (input (this <search-view>) char)
  (case char
    ((#\q) '(pop))))

(app/define-method (main-loop date)
  (define state (list (day-view (app/getf 'event-set) date)))

  (while #t
    (output (car state))

    (let ((char (read-char)))

      (when (eof-object? char)
        (break))

      (match (input (car state) char)
        (('push new-state) (set! state (cons new-state state)))
        (('pop)
         (set! state (cdr state))
         (when (null? state) (break)))
        (('break) (break))
        (else
         'continue)))))

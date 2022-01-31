(define-module (calp terminal)
  #:use-module (srfi srfi-1)
  #:use-module (datetime)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-26)
  #:use-module ((srfi srfi-41) :select (stream-car))
  #:use-module (hnh util)
  #:use-module (vulgar)
  #:use-module (vulgar info)
  #:use-module (vulgar color)
  #:use-module (vulgar components)

  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (vcomponent util search)
  #:use-module (vcomponent util group)

  #:use-module (text util)
  #:use-module (text flow)

  #:use-module (ice-9 format)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 match)

  #:use-module (vulgar termios)

  #:use-module (oop goops)
  #:use-module (oop goops describe)

  #:autoload (vcomponent util instance) (global-event-object)

  #:export (main-loop))

(define-values (height width) (get-terminal-size))

(define (open-in-editor fname)
  (system (string-append (getenv "EDITOR") " " fname)))


(define (box-top intersection line . lengths)
  (reduce (lambda (str done) (string-append done (string intersection) str))
          "" (map (cut make-string <> line) lengths)))

(define* (display-event-table events #:key
                              (active-element -1)
                              ;; (summary-width 30)
                              (date-width 17)
                              (location-width 20))
  (define summary-width (- width date-width location-width 6))

  (displayln
   (box-top #\┬ #\─ date-width (+ summary-width 2) (1+ location-width)))
  (for-each
   (lambda (ev i)
     (display
      (string-append
       (if (datetime? (prop ev 'DTSTART))
           (datetime->string (prop ev 'DTSTART) "~Y-~m-~d ~H:~M")
           (date->string (prop ev 'DTSTART) "~Y-~m-~d --:--"))
       " │ "
       (if (= i active-element) "\x1b[7m" "")
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
   (iota (length events)))
  (displayln
   (box-top #\┴ #\─ date-width (+ summary-width 2) (1+ location-width))))

(define (displayln a)
  (display a) (newline))


(define-class <view> ()
  (event-set getter: get-event-set init-keyword: event-set:)
  (active-element accessor: active-element
                  init-value: 0)
  (current-page accessor: current-page
                init-keyword: current-page:)
  (page-length accessor: page-length
               init-value: 0)

  )


(define-class <day-view> (<view>)
  #;
  (date accessor: view-date
        init-keyword: date:)

  (cached-events accessor: cached-events
                 init-value: #f)
  (groups accessor: groups))


(define-method (initialize (this <day-view>) args)
  (next-method)
  (set! (groups this) (group-stream (get-event-set this))))

(define-method (output (this <day-view>))

  (define events
    (aif (cached-events this)
         it
         (set/r! (cached-events this)
                 (group->event-list (stream-car (get-groups-between
                                                 (groups this)
                                                 (current-page this)
                                                 (current-page this)))))))

  (cls)

  (display "== Day View ==\n")

  (display-calendar-header! (current-page this))

  ;; display event list
  (display-event-table
   events
   active-element: (active-element this)
   location-width: 15)

  ;; display highlighted event
  (unless (null? events)
    (let ((ev (list-ref events (active-element this))))
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

(define (get-line prompt)
  (let* ((attr (make-termios))
         (input-string #f))
    (tcgetattr! attr)
    (set! (lflag attr) (logior ECHO (lflag attr)))
    (tcsetattr! attr)
    (system "tput cnorm")
    (set! input-string (readline prompt))
    (system "tput civis")
    (set! (lflag attr) (logand (lognot ECHO) (lflag attr)))
    (tcsetattr! attr)
    input-string
    ))

(define-method (input (this <day-view>) char)
  (set! (page-length this) (length (cached-events this)))

  (case char
    ((#\L #\l right)
     (set! (current-page this) = add-day
           (cached-events this) #f
           (active-element this) 0))

    ((#\h #\H left)
     (set! (current-page this) = remove-day
           (cached-events this) #f
           (active-element this) 0))

    ((#\t)
     ;; TODO this should be local time
     ;; currently it's UTC (maybe?)
     (set! (current-page this) (current-date)
           (cached-events this) #f
           (active-element this) 0))

    ((#\/) (set-cursor-pos 0 (1- height))
     (let ((search-term (get-line "quick search: ")))
       `(push ,(search-view
                (format #f "(regexp-exec (make-regexp \"~a\" regexp/icase) (prop event 'SUMMARY))"
                        search-term)
                (get-event-set this)))))

    ((#\() (set-cursor-pos 0 (1- height))
     (let ((search-term (get-line "search: ")))
       `(push ,(search-view search-term (get-event-set this)))))

    (else (next-method))))

(define (day-view event-set date)
  (make <day-view> event-set: event-set current-page: date))

(define-class <search-view> (<view>)
  (search-result getter: search-result)
  (search-term accessor: search-term
               init-keyword: search-term:))

(define (search-view search-term event-set)
  (make <search-view> search-term: search-term event-set: event-set))


(define-method (initialize (this <search-view>) args)
  (set! (current-page this) 0)
  (next-method)

  (set! (search-term this)
    (prepare-string (search-term this)))

  (let ((q (build-query-proc (search-term this))))
    (slot-set! this 'search-result
               (prepare-query
                q (get-event-set this))))
  ;; (define current-page 0)
  ;; (define current-entry 0)
  )

(define-method (output (this <search-view>))

  (define paginator (slot-ref this 'search-result))

  (define page
    (catch 'max-page
      (lambda () (get-page paginator (current-page this)))
      (lambda (err page-number)
        (set! (current-page this) page-number)
        (get-page paginator page-number))))


  (cls)

  (display "== Search View ==\n")

  ;; display search term
  (format #t "~y" (search-term this))

  ;; display event list
  (display-event-table
   page
   #:active-element (active-element this)
   #:location-width 15)

  (paginator->sub-list
   paginator (current-page this)
   (lambda (i)
     (if (= i (current-page this))
         (format #t "[~2@a]" i)
         (format #t " ~2@a " i)))
   head-proc:
   (lambda (start)
     (if (= start 0)
         "|" "<"))
   tail-proc:
   (lambda (end)
     (if (= end (get-max-page paginator))
         (if (true-max-page? paginator)
             "|" "?")
         ">")))
  (newline))

(define-method (input (this <view>) char)
  (case char
    ((#\j #\J down) (unless (= (active-element this) (1- (page-length this)))
                      (set! (active-element this) = (+ 1))))
    ((#\k #\K up) (unless (= (active-element this) 0)
                    (set! (active-element this) = (- 1))))

    ((#\g) (set! (active-element this) 0))
    ((#\G) (set! (active-element this) (1- (page-length this))))


    ((#\q) '(pop)))

  )

(define-method (input (this <search-view>) char)
  ;; TODO update this to match actual page length
  (set! (page-length this) 10)

  (case char
    ((#\newline) `(push ,(day-view (get-event-set this)
                                   (as-date (prop (list-ref (get-page (slot-ref this 'search-result)
                                                                      (current-page this))
                                                           (active-element this))
                                                 'DTSTART)))))
    ((#\h left) (set! (current-page this) = ((lambda (old) (max 0 (1- old))))))
    ((#\l right)
     (display "\n loading...\n")
     (set! (current-page this)
       (next-page (slot-ref this 'search-result)
                  (current-page this))))
    (else (next-method))))

(define-public (main-loop date)
  (define state (list (day-view (get-event-set global-event-object) date)))

  (while #t
    (output (car state))

    (let ((char (read-char)))
      (when (eof-object? char)
        (break))

      (when (char=? char #\escape)
        (case (read-char)
          ((#\[)
           (case (read-char)
             ((#\A) (set! char 'up))
             ((#\B) (set! char 'down))
             ((#\C) (set! char 'right))
             ((#\D) (set! char 'left))))))

      (match (input (car state) char)
        (('push new-state) (set! state (cons new-state state)))
        (('pop)
         (set! state (cdr state))
         (when (null? state) (break)))
        (('break) (break))
        (else 'continue)))))

(define-module (entry-points server)
  :use-module (util)
  :use-module (util app)
  :use-module (util config)
  :use-module (util options)

  :use-module (srfi srfi-1)

  :use-module (ice-9 match)
  :use-module (ice-9 control)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 ftw)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 iconv)
  :use-module (ice-9 regex) #| regex here due to bad macros |#

  :use-module (web server)
  :use-module (web request)
  :use-module (web response)
  :use-module (web uri)
  :use-module (web http)

  :use-module (sxml simple)

  :use-module (server util)
  :use-module (server macro)

  :use-module (vcomponent)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (output html)
  :use-module (output ical)

  :export (main)
  )

(define (file-extension name)
  (car (last-pair (string-split name #\.))))

(define (sxml->html-string sxml)
  (with-output-to-string
    (lambda () (display "<!doctype html>\n") (sxml->xml sxml))))

(define (// . args) (string-join args file-name-separator-string ))

(define (directory-table dir)
  `(table
    (thead
     (tr (th "") (th "Name") (th "Perm")))
    (tbody
     ,@(map (lambda (k)
              (let* ((stat (lstat (// dir k))))
                `(tr (td ,(case (stat:type stat)
                            [(directory) "📁"]
                            [(regular) "📰"]
                            [else "🙃"]))
                     (td (a (@ (href "/" ,dir "/" ,k)) ,k))
                     (td ,(number->string (stat:perms stat) 8)))))
            (cdr (scandir dir))))))


(define-method (make-make-routes)
  (make-routes

   ;; Manual redirect to not reserve root.
   (GET "/" ()
        (return '((content-type text/html))
                (sxml->html-string '(a (@ (href "/today")) "Gå till idag"))))

   (GET "/week/:start-date.html" (start-date)
        (let* ((start-date
                (start-of-week (parse-iso-date start-date)
                               (get-config 'week-start))))

          (return '((content-type text/html))
                  (with-output-to-string
                    (lambda ()
                      (html-generate calendars: (getf 'calendars)
                                     events: (getf 'event-set)
                                     start-date: start-date
                                     end-date: (date+ start-date (date day: 6))
                                     next-start: (lambda (d) (date+ d (date day: 7)))
                                     prev-start: (lambda (d) (date- d (date day: 7)))
                                     render-calendar: render-calendar
                                     intervaltype: 'week
                                     ))))))

   (GET "/month/:start-date.html" (start-date)
        (let* ((start-date (start-of-month (parse-iso-date start-date))))

          (return '((content-type text/html))
                  (with-output-to-string
                    (lambda ()
                      (html-generate calendars: (getf 'calendars)
                                     events: (getf 'event-set)
                                     start-date: start-date
                                     end-date: (date- (month+ start-date)
                                                      (date day: 1))
                                     next-start: month+
                                     prev-start: month-
                                     render-calendar: render-calendar-table
                                     pre-start: (start-of-week start-date (get-config 'week-start))
                                     post-end: (end-of-week (end-of-month start-date) (get-config 'week-start))
                                     intervaltype: 'month
                                     ))))))

   ;; Get specific page by query string instead of by path.
   ;; Useful for <form>'s, since they always submit in this form, but also
   ;; useful when javascript is disabled, since a link to "today" needs some
   ;; form of evaluation when clicked.
   (GET "/today" (view date)
        (define location
          (parse-header 'location
                        (format #f "/~a/~a.html"
                                (or view "month")
                                (date->string
                                 (cond [date => parse-iso-date]
                                       [else (current-date)])
                                 "~1"))) )

        (return (build-response
                 code: 302
                 headers: `((location . ,location)))
                ""))

   (GET "/calendar" (start end)
        (return '((content-type text/calendar))
                (with-output-to-string
                  (lambda ()
                    (if (or start end)
                        (print-events-in-interval
                         (aif start (parse-iso-date it) (current-date))
                         (aif end (parse-iso-date it) (current-date)))
                        (print-all-events))))))

   (GET "/calendar/:uid{.*}.ics" (uid)
        (aif (get-event-by-uid uid)
             (return '((content-type text/calendar))
                     (with-output-to-string
                       (lambda () (print-components-with-fake-parent
                              (list it)))))
             (return (build-response code: 404)
                     (format #f "No component with UID=~a found." uid))))

   ;; NOTE this only handles files with extensions. Limited, but since this
   ;; is mostly for development, and something like nginx should be used in
   ;; production it isn't a huge problem.

   (GET "/static/:*{.*}.:ext" (* ext)
        (return
         ;; TODO actually check mimetype
         `((content-type ,(string->symbol (string-append "text/" ext))))
         (call-with-input-file (string-append "static/" * "." ext)
           read-string)))

   (GET "/static/:*{.*}" (*)
        (return
         '((content-type text/html))
         (sxml->html-string
          (directory-table (// "static" *)))))


   (GET "/count" ()
        ;; (sleep 1)
        (return '((content-type text/plain))
                (string-append (number->string state) "\n")
                (1+ state)))))

(define options
  '((port (value #t) (single-char #\p)
          (description (*TOP* "Bind to TCP port, defaults to " (i 8080) ".")))
    (addr (value #t)
          (description (*TOP* "Address to use, defaults to " (i "0.0.0.0")
                              " for IPv4, and " (i "::") " for IPv6."))
          )
    ;; TODO numbers as single-char seems to not work.
    (six (single-char #\6)
         (description "Use IPv6."))
    (four (single-char #\4)
          (description "Use IPv4."))
    (help (single-char #\h)
          (description "Print this help."))))


(define-public (main args)

  (define opts (getopt-long args (getopt-opt options)))
  (define port (string->number (option-ref opts 'port "8080")))
  (define addr (option-ref opts 'addr #f))
  (define family
    (cond [(option-ref opts 'six  #f) AF_INET6]
          [(option-ref opts 'four #f) AF_INET]
          [(and addr (string-contains addr ":")) AF_INET6]
          [(and addr (string-contains addr ".")) AF_INET]
          [else AF_INET6]))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  ;; update address if it was left blank. A bit clumsy since
  ;; @var{addr} & @var{family} depend on each other.
  ;; placed after load-calendars to keep Guile 2.2 compability.
  (set! addr
    (if addr addr
        (if (eqv? family AF_INET6)
            "::" "0.0.0.0")))

  ;; NOTE The default make-default-socket is broken for IPv6.
  ;; A patch has been submitted to the mailing list. 2020-03-31
  (module-set!
   (resolve-module '(web server http))
   'make-default-socket
   (lambda (family addr port)
     (let ((sock (socket family SOCK_STREAM 0)))
       (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
       (bind sock family addr port)
       sock)))


  ;; TODO possibly test inet-pton here on address?

  (format #t "Starting server on ~a:~a~%I'm ~a, runing from ~a~%"
          addr port
          (getpid) (getcwd))

  (catch 'system-error
    (lambda ()
     (run-server (make-make-routes)
                 'http
                 `(family: ,family
                           port: ,port
                           host: ,addr)
                 0))
    ;; probably address already in use
    (lambda (err proc fmt args errno)
      (format (current-error-port) "~a: ~?~%"
              proc fmt args))))

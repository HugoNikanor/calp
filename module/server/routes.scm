(define-module (server routes)
  :use-module (util)
  :use-module (util options)
  :use-module (util exceptions)

  :use-module (srfi srfi-1)

  :use-module ((ice-9 rdelim) :select (read-string))
  :use-module ((ice-9 ftw) :select (scandir))
  :use-module (ice-9 regex) #| regex here due to bad macros |#

  :use-module ((web response) :select (build-response))
  :use-module ((web uri) :select (build-relative-ref))

  :use-module (sxml simple)
  :use-module (sxml xpath)
  :use-module (sxml namespace)


  :use-module ((html util) :select (html-unattr))

  :use-module (server util)
  :use-module (server macro)

  :use-module (vcomponent)
  :use-module (vcomponent search)
  :use-module (datetime)
  ;; :use-module (output html)
  :use-module (output ical)

  :autoload (vcomponent instance) (global-event-object)

  :use-module (html view calendar)
  :use-module ((html view search) :select (search-result-page))


  )



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



(define get-query-page
  ;; A user of the website is able to fill up all of the hosts memory by
  ;; requesting a bunch of different search pages, and forcing a bunch
  ;; of pages on each. Clean up this table from time to time, possibly
  ;; by popularity-rank.
  (let ((query-pages (make-hash-table)))
   (lambda (search-term)
     (aif (hash-ref query-pages search-term)
          it
          (let* ((q (prepare-query
                     (build-query-proc search-term)
                     (get-event-set global-event-object))))
            (hash-set! query-pages search-term q)
            q)))))




;; TODO ensure encoding on all fields which take user provided data.
;; Possibly a fallback which strips everything unknown, and treats
;; the bytevector as ascii.
(define-public (make-make-routes)
  (make-routes

   ;; Manual redirect to not reserve root.
   (GET "/" ()
        (return '((content-type text/html))
                (sxml->html-string '(a (@ (href "/today")) "Gå till idag"))))

   (GET "/favicon.ico" ()
        (return
         `((content-type image/svg+xml))
         (call-with-input-file "static/calendar.svg" read-string)))

   ;; TODO any exception in this causes the whole page to fail
   ;; It would be much better if most of the page could still make it.
   (GET "/week/:start-date.html" (start-date)
        (let* ((start-date
                (start-of-week (parse-iso-date start-date))))

          (return `((content-type application/xhtml+xml))
                  (with-output-to-string
                    (lambda ()
                      (html-generate calendars: (get-calendars global-event-object)
                                     events: (get-event-set global-event-object)
                                     start-date: start-date
                                     end-date: (date+ start-date (date day: 6))
                                     next-start: (lambda (d) (date+ d (date day: 7)))
                                     prev-start: (lambda (d) (date- d (date day: 7)))
                                     render-calendar: (@ (html view calendar week) render-calendar)
                                     intervaltype: 'week
                                     ))))))

   (GET "/month/:start-date.html" (start-date)
        (let* ((start-date (start-of-month (parse-iso-date start-date))))

          (return '((content-type application/xhtml+xml))
                  (with-output-to-string
                    (lambda ()
                      (html-generate calendars: (get-calendars global-event-object)
                                     events: (get-event-set global-event-object)
                                     start-date: start-date
                                     end-date: (date- (month+ start-date)
                                                      (date day: 1))
                                     next-start: month+
                                     prev-start: month-
                                     render-calendar: (@ (html view calendar month)
                                                         render-calendar-table)
                                     pre-start: (start-of-week start-date)
                                     post-end: (end-of-week (end-of-month start-date))
                                     intervaltype: 'month
                                     ))))))

   (POST "/remove" (uid)
         (unless uid
           (return (build-response code: 400)
                   "uid required"))

         (aif (get-event-by-uid global-event-object uid)
              (begin
                ;; It's hard to properly remove a file. I also want a way to undo accidental
                ;; deletions. Therefore I simply save the X-HNH-REMOVED flag to the file, and
                ;; then simple don't use those events when loading.
                (remove-event global-event-object it)
                (set! (prop it 'X-HNH-REMOVED) #t)
                (set! (param (prop* it 'X-HNH-REMOVED) 'VALUE) "BOOLEAN")
                (unless ((@ (output vdir) save-event) it)
                  (return (build-response code: 500)
                          "Saving event to disk failed."))
                (return (build-response code: 204)))
              (return (build-response code: 400)
                      (format #f "No event with UID '~a'" uid))))

   ;; TODO this fails when dtstart is <date>.
   ;; @var{cal} should be the name of the calendar encoded with
   ;; modified base64. See (html util).
   (POST "/insert" (cal data)

         (unless (and cal data)
           (return (build-response code: 400)
                   "Both 'cal' and 'data' required\r\n"))


         ;; NOTE that this leaks which calendar exists,
         ;; but you can only query for existance.
         ;; also, the calendar view already show all calendars.
         (let* ((calendar-name (html-unattr cal))
                (calendar
                  (find (lambda (c) (string=? calendar-name (prop c 'NAME)))
                        (get-calendars global-event-object))))

           (unless calendar
             (return (build-response code: 400)
                     (format #f "No calendar with name [~a]\r\n" calendar-name)))

           ;; Expected form of data (but in XML) is:
           ;; @example
           ;; (*TOP*
           ;;  (*PI* ...)
           ;;  (icalendar (@ (xmlns "..."))
           ;;   (vcalendar
           ;;    (vevent ...))))
           ;; @end example
           ;; However, *PI* will probably be omited, and currently events
           ;; are sent without the vcalendar part. Earlier versions
           ;; Also omitted the icalendar part. And I'm not sure if the
           ;; *TOP* node is a required part of the sxml.

           (let ((event
                   ((@ (vcomponent parse xcal) sxcal->vcomponent)
                    (catch 'parser-error
                      (lambda ()
                        (move-to-namespace
                         ;; TODO Multiple event components
                         (car ((sxpath '(// IC:vevent))
                               (xml->sxml data namespaces: '((IC . "urn:ietf:params:xml:ns:icalendar-2.0")))))
                         #f))
                      (lambda (err port . args)
                        (return (build-response code: 400)
                                (format #f "XML parse error ~{~a~}\r\n" args)))))))

             (unless (eq? 'VEVENT (type event))
               (return (build-response code: 400)
                       "Object not a VEVENT\r\n"))

             ;; NOTE add-event uses the given UID if one is given,
             ;; but generates its own if not. It might be a good idea
             ;; to require that UID is unset here, and force users
             ;; to use a /update endpoint to change events. This to prevent
             ;; accidental overwriting.


             (cond
              [(get-event-by-uid global-event-object (prop event 'UID))
               => (lambda (old-event)

                    (if (eq? calendar (parent old-event))
                        (begin (vcomponent-update! old-event event)
                               ;; for save below
                               (set! event old-event))

                        ;; change calendar
                        (begin
                          ;; (remove-from-calendar! old-event)
                          ;; TODO remove the old event from disk here
                          (remove-event global-event-object old-event)

                          (parameterize ((warnings-are-errors #t))
                            (catch 'warning
                              (lambda () (add-event global-event-object calendar event))
                              (lambda (err fmt args)
                                (return (build-response code: 400)
                                        (format #f "~?~%" fmt args)))))))


                    ;; NOTE Posibly defer save to a later point.
                    ;; That would allow better asyncronous preformance.
                    (unless ((@ (output vdir) save-event) event)
                      (return (build-response code: 500)
                              "Saving event to disk failed."))


                    (format (current-error-port)
                            "Event updated ~a~%" (prop event 'UID)))]

              [else
               (parameterize ((warnings-are-errors #t))
                 (catch 'warning
                   (lambda () (add-event global-event-object calendar event))
                   (lambda (err fmt args)
                     (return (build-response code: 400)
                             (format #f "~?~%" fmt args)))))

               ;; NOTE Posibly defer save to a later point.
               ;; That would allow better asyncronous preformance.
               (unless ((@ (output vdir) save-event) event)
                 (return (build-response code: 500)
                         "Saving event to disk failed."))

               (format (current-error-port)
                       "Event inserted ~a~%" (prop event 'UID))])

             (return '((content-type application/xml))
                     (with-output-to-string
                       (lambda ()
                         (sxml->xml
                          `(properties
                            (uid (text ,(prop event 'UID)))))))))))

   ;; Get specific page by query string instead of by path.
   ;; Useful for <form>'s, since they always submit in this form, but also
   ;; useful when javascript is disabled, since a link to "today" needs some
   ;; form of evaluation when clicked.
   (GET "/today" (view date)
        (define location
          (build-relative-ref
           path:
           (format #f "/~a/~a.html"
                   (or view "week")
                   (date->string
                    (cond [date => parse-iso-date]
                          [else (current-date)])
                    "~1"))) )

        (return (build-response
                 code: 302
                 headers: `((location . ,location)))))

   (GET "/calendar" (start end)
        (return '((content-type text/calendar))
                (with-output-to-string
                  (lambda ()
                    (if (or start end)
                        (print-events-in-interval
                         (aif start (parse-iso-date it) (current-date))
                         (aif end (parse-iso-date it) (current-date)))
                        (print-all-events))))))

   (GET "/calendar/:uid{.*}.xcs" (uid)
        (aif (get-event-by-uid global-event-object uid)
             (return '((content-type application/calendar+xml))
                     ;; TODO sxml->xml takes a port, would be better
                     ;; to give it the return port imidiately.
                     (with-output-to-string
                       ;; TODO this is just the vevent part.
                       ;; A surounding vcalendar is required, as well as
                       ;; a doctype.
                       ;; Look into changing how events carry around their
                       ;; parent information, possibly splitting "source parent"
                       ;; and "program parent" into different fields.
                       (lambda () (sxml->xml ((@ (output xcal) vcomponent->sxcal) it)))))
             (return (build-response code: 404)
                     (format #f "No component with UID=~a found." uid))))

   (GET "/calendar/:uid{.*}.ics" (uid)
        (aif (get-event-by-uid global-event-object uid)
             (return '((content-type text/calendar))
                     (with-output-to-string
                       (lambda () (print-components-with-fake-parent
                              (list it)))))
             (return (build-response code: 404)
                     (format #f "No component with UID=~a found." uid))))

   ;; TODO search without query should work
   (GET "/search" (q p)
        (define search-term (prepare-string q))

        (define q= (find (lambda (s)
                           (and (<= 2 (string-length s))
                                (string=? "q=" (string-take s 2))))
                         (string-split r:query #\&)))

        (define paginator (get-query-page search-term))

        (define page (string->number (or p "0")))

        ;; TODO Propagate errors
        (define search-result
          (catch 'max-page
            ;; TODO Get-page only puts a time limiter per page, meaning that
            ;; if a user requests page 1000 the server is stuck trying to
            ;; find that page, which can take up to 1000 * timeslice = 500s = 8min+
            ;; A timeout here, and also an actual multithreaded server should
            ;; solve this.
            (lambda () (get-page paginator page))
            (lambda (err page-number)
              (define location
                (build-relative-ref
                 path: r:path ; host: r:host port: r:port
                 query: (format #f "~a&p=~a" q= page-number)))
              (return (build-response
                       code: 307
                       headers: `((location . ,location)))))))

        (return '((content-type application/xhtml+xml))
                (with-output-to-string
                  (lambda ()
                    (sxml->xml
                     (search-result-page
                      search-term search-result page paginator q=))))))

   ;; NOTE this only handles files with extensions. Limited, but since this
   ;; is mostly for development, and something like nginx should be used in
   ;; production it isn't a huge problem.

   (GET "/static/:*{.*}.:ext" (* ext)

        ;; Actually parsing /etc/mime.types would be better.
        (define mime
          (case (string->symbol ext)
            [(js) "javascript"]
            [else ext]))

        (return
         `((content-type ,(string->symbol (string-append "text/" mime))))
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

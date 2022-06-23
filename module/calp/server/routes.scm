(define-module (calp server routes)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (hnh util exceptions)

  :use-module (srfi srfi-1)

  :use-module ((ice-9 rdelim) :select (read-string))
  :use-module ((ice-9 ftw) :select (scandir))
  :use-module (ice-9 format)

  :use-module ((web response) :select (build-response))
  :use-module ((web uri) :select (build-relative-ref))
  :use-module ((web uri-query) :select (encode-query-parameters))

  :use-module ((sxml simple) :select (sxml->xml xml->sxml))
  :use-module ((sxml html)   :select (sxml->html))
  :use-module (sxml xpath)
  :use-module (sxml namespace)

  :use-module ((rnrs io ports) :select (get-bytevector-all))
  :use-module ((xdg basedir) :prefix xdg-)

  :use-module ((base64) :select (base64decode))

  :use-module (web http make-routes)

  :use-module (vcomponent)
  :use-module (vcomponent util search)
  :use-module (datetime)
  :use-module (vcomponent formats ical output)

  :autoload (vcomponent util instance) (global-event-object)

  :use-module (calp util config)
  :use-module (calp html view calendar)
  :use-module ((calp html view search) :select (search-result-page))

  :use-module (calp translation)

  :use-module ((calp html components) :select (xhtml-doc include-css))

  :export (make-make-routes)
  )



(define (content-type html?)
  (if html? 'text/html 'application/xhtml+xml))

(define (sxml->output html?)
  (if html? sxml->html sxml->xml))



;; @var{prefix} directory tree which should be exported
;; @var{dir}    location in exported directory tree
;; Note that the exported url is currently hard-coded to
;; start with /static.
(define (directory-table prefix dir)
  `(table (@ (class "directory-table"))
          (thead
           (tr (th "")
               (th ,(_ "Name"))
               ;; File permissions, should be about as long as three digits
               (th ,(_ "Perm"))
               ;; File size
               (th ,(_ "Size"))))
    (tbody
     (tr (td "↩️") (td (@ (colspan 3))
                      (a (@ (href ,(-> (path-split dir)
                                       (drop-right 1)
                                       (xcons "/static")
                                       path-join)))
                         ,(_ "Return up"))))
     ,@(map (lambda (k)
              (let ((stat (lstat (path-append prefix dir k))))
                `(tr (td ,(case (stat:type stat)
                            [(directory) "📁"]
                            [(regular) "📰"]
                            [(symlink) "🔗"]
                            [(block-special) "🖴"]
                            [(char-special) "🔌"]
                            ;; [(fifo)]
                            ;; [(socket)]
                            [else "🙃"]))
                     (td (a (@ (href ,(path-append "/static" dir k)))
                            ,k))
                     (td ,(number->string (stat:perms stat) 8))
                     (td (@ (style "text-align:end"))
                         (data (@ (value ,(stat:size stat)))
                               ,(format #f "~:d" (stat:size stat)))))))
            ;; cddr drops '.' and '..'
            (cddr (or (scandir (path-append prefix dir))
                      (scm-error
                       'misc-error
                       "directory-table"
                       (_ "Scandir argument invalid or not directory: ~s")
                       (list dir) '())))))))



(define get-query-page
  ;; A user of the website is able to fill up all of the hosts memory by
  ;; requesting a bunch of different search pages, and forcing a bunch
  ;; of pages on each. Clean up this table from time to time, possibly
  ;; by popularity-rank.
  (let ((query-pages (make-hash-table)))
   (lambda (search-term)
     (aif (hash-ref query-pages search-term)
          it
          (let ((q (prepare-query
                     (build-query-proc search-term)
                     (get-event-set global-event-object))))
            (hash-set! query-pages search-term q)
            q)))))



(define-config static-dir "static"
  description: (_ "Where static files for the web server are located"))


(define ical-namespace '(IC . "urn:ietf:params:xml:ns:icalendar-2.0"))


(define root-script "window.onload = () => document.getElementsByTagName('a')[0].click()")

;; TODO ensure encoding on all fields which take user provided data.
;; Possibly a fallback which strips everything unknown, and treats
;; the bytevector as ascii.
(define (make-make-routes)
  (make-routes

   ;; Manual redirect to not reserve root.
   ;; Also reason for really ugly frontend redirect.
   (GET "/" (html)
        (return `((content-type ,(content-type html)))
                (with-output-to-string
                  (lambda ()
                   ((sxml->output html)
                    (xhtml-doc
                     (body (a (@ (href "/today")) ,(_ "Go to Today"))
                           (script ,(lambda () (display root-script))))))))))

   (GET "/favicon.ico" ()
        (return
         `((content-type image/svg+xml))
         (call-with-input-file "static/calendar.svg" read-string)))

   ;; TODO any exception in this causes the whole page to fail
   ;; It would be much better if most of the page could still make it.
   (GET "/week/:start-date.html" (start-date html)
        (let ((start-date (start-of-week (parse-iso-date start-date))))
          (return `((content-type ,(content-type html)))
                  (with-output-to-string
                    (lambda ()
                      ((sxml->output html)
                       (html-generate calendars: (get-calendars global-event-object)
                                      events: (get-event-set global-event-object)
                                      start-date: start-date
                                      end-date: (date+ start-date (date day: 6))
                                      next-start: (lambda (d) (date+ d (date day: 7)))
                                      prev-start: (lambda (d) (date- d (date day: 7)))
                                      render-calendar: (@ (calp html view calendar week) render-calendar)
                                      intervaltype: 'week
                                      )))))))

   (GET "/month/:start-date.html" (start-date html)
        (let ((start-date (start-of-month (parse-iso-date start-date))))
          (return `((content-type ,(content-type html)))
                  (with-output-to-string
                    (lambda ()
                      ((sxml->output html)
                       (html-generate calendars: (get-calendars global-event-object)
                                      events: (get-event-set global-event-object)
                                      start-date: start-date
                                      end-date: (date- (month+ start-date)
                                                       (date day: 1))
                                      next-start: month+
                                      prev-start: month-
                                      render-calendar: (@ (calp html view calendar month)
                                                          render-calendar-table)
                                      pre-start: (start-of-week start-date)
                                      post-end: (end-of-week (end-of-month start-date))
                                      intervaltype: 'month
                                      )))))))

   (POST "/remove" (uid)
         (unless uid
           (return (build-response code: 400)
                   (_ "uid required")))

         (aif (get-event-by-uid global-event-object uid)
              (begin
                ;; It's hard to properly remove a file. I also want a way to undo accidental
                ;; deletions. Therefore I simply save the X-HNH-REMOVED flag to the file, and
                ;; then simple don't use those events when loading.
                (remove-event global-event-object it)
                (set! (prop it 'X-HNH-REMOVED) #t)
                (set! (param (prop* it 'X-HNH-REMOVED) 'VALUE) "BOOLEAN")
                (unless ((@ (vcomponent formats vdir save-delete) save-event) it)
                  (return (build-response code: 500)
                          (_ "Saving event to disk failed.")))
                (return (build-response code: 204)))
              (return (build-response code: 400)
                      (format #f (_ "No event with UID '~a'") uid))))

   ;; TODO this fails when dtstart is <date>.
   ;; @var{cal} should be the name of the calendar encoded in base64.
   (POST "/insert" (cal data)

         (unless (and cal data)
           (return (build-response code: 400)
                   (string-append (_ "Both 'cal' and 'data' required") "\r\n")))

         ;; NOTE that this leaks which calendar exists,
         ;; but you can only query for existance.
         ;; also, the calendar view already show all calendars.
         (let* ((calendar-name (base64decode cal))
                (calendar
                 (get-calendar-by-name global-event-object calendar-name)))

           (unless calendar
             (return (build-response code: 400)
                     (format #f "~@?\r\n" (_ "No calendar with name [~a]")
                             calendar-name)))

           ;; Expected form of data (but in XML) is:
           ;; @example
           ;; (*TOP*
           ;;  (*PI* ...)
           ;;  (icalendar (@ (xmlns "..."))
           ;;   (vcalendar
           ;;    (vevent ...))))
           ;; @end example

           ;; TODO
           ;; However, *PI* will probably be omited, and currently events
           ;; are sent without the vcalendar part. Earlier versions
           ;; Also omitted the icalendar part. And I'm not sure if the
           ;; *TOP* node is a required part of the sxml.

           (let ((event
                   ((@ (vcomponent formats xcal parse) sxcal->vcomponent)
                    (catch 'parser-error
                      (lambda ()
                        (-> data
                            (xml->sxml namespaces: (list ical-namespace))
                            ((sxpath '(// IC:vevent)))
                            ;; TODO Multiple event components
                            car
                            (move-to-namespace #f)))
                      (lambda (err port . args)
                        (return (build-response code: 400)
                                (format #f "~a ~{~a~}\r\n"
                                        (_ "XML parse error")
                                        args)))))))

             (unless (eq? 'VEVENT (type event))
               (return (build-response code: 400)
                       (string-append (_ "Object not a VEVENT") "\r\n")))

             ;; NOTE add-event uses the given UID if one is given,
             ;; but generates its own if not. It might be a good idea
             ;; to require that UID is unset here, and force users
             ;; to use a /update endpoint to change events. This to prevent
             ;; accidental overwriting.


             (parameterize ((warnings-are-errors #t))
               (catch*
                (lambda () (add-and-save-event global-event-object
                                          calendar event))
                (warning
                 (lambda (err fmt args)
                   (define str (format #f "~?" fmt args))
                   (format (current-error-port) "400 ~a~%" str)
                   (return (build-response code: 400)
                           str)))
                (#t
                 (lambda (err proc fmt args _)
                   (define str (format #f "~a in ~a: ~?~%" err proc fmt args))
                   (format (current-error-port) "500 ~a~%" str)
                   (return (build-response code: 500)
                           str)))))

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
                       (lambda () (sxml->xml ((@ (vcomponent formats xcal output) vcomponent->sxcal) it)))))
             (return (build-response code: 404)
                     (format #f (_ "No component with UID=~a found.") uid))))

   (GET "/calendar/:uid{.*}.ics" (uid)
        (aif (get-event-by-uid global-event-object uid)
             (return '((content-type text/calendar))
                     (with-output-to-string
                       (lambda () (print-components-with-fake-parent
                              (list it)))))
             (return (build-response code: 404)
                     (format #f (_ "No component with UID=~a found.") uid))))

   (GET "/search/text" (q)
        (return (build-response
                 code: 302
                 headers:
                 `((location
                    . ,(build-relative-ref
                        path: "/search/"
                        query:
                        (encode-query-parameters
                         `((q . (regexp-exec
                                 (make-regexp ,(->string q)
                                              regexp/icase)
                                 (prop event 'SUMMARY)))))
                        ))))))

   (GET "/search" (q p onlyfuture html)
        (define search-term
          (if (and q (not (string-null? q)))
              (if onlyfuture
                  `(and (date/-time<=? ,(current-datetime) (prop event 'DTSTART))
                        ,(and=> q prepare-string))
                  (and=> q prepare-string))
              ;; NOTE This causes the paginator buttons to search for literally two quote marks,
              ;; But oh well.
              ""))

        ;; get-query-page handles paginator cache, meaning that
        ;; a new one is only allocated when needed
        (define paginator (get-query-page search-term))

        (define page (string->number (or p "0")))

        (define error #f)

        (define search-result
          ;; TODO Get-page only puts a time limiter per page, meaning that
          ;; if a user requests page 1000 the server is stuck trying to
          ;; find that page, which can take up to 1000 * timeslice = 500s = 8min+
          ;; A timeout here, and also an actual multithreaded server should
          ;; solve this.
          (catch* (lambda () (get-page paginator page))
                  (max-page
                   (lambda (err page-number)
                     (define location
                       (build-relative-ref
                        path: r:path        ; host: r:host port: r:port
                        query: (encode-query-parameters
                                `((p . ,page-number)
                                  (q . ,search-term)))))
                     (return (build-response
                              code: 307
                              headers: `((location . ,location))))))
                  (#t
                   (lambda (err callee fmt arg data)
                     (set! error
                       (format #f "~?~%" fmt arg))))))

        (return `((content-type (content-type html)))
                (with-output-to-string
                  (lambda ()
                    ((sxml->output html)
                     (search-result-page
                      error
                      (and=> q (negate string-null?))
                      search-term search-result page paginator))))))

   ;; NOTE this only handles files with extensions. Limited, but since this
   ;; is mostly for development, and something like nginx should be used in
   ;; production it isn't a huge problem.


   (GET "/static/:*{.*}.:ext" (* ext)

        ;; Actually parsing /etc/mime.types would be better.
        (define mime
          (case (string->symbol ext)
            [(js) "javascript"]
            [else ext]))

        (catch 'system-error
          (lambda ()
           (return
            `((content-type ,(string->symbol (string-append "text/" mime))))
            (call-with-input-file (path-append (static-dir) (string-append * "." ext))
              read-string)))
          (lambda (err proc fmt fmt-args data)
            (warning (format #f "404|500: ~?" fmt fmt-args))
            (if (= ENOENT (car data))
                (return (build-response code: 404)
                        (format #f "~?" fmt fmt-args))
                (scm-error err proc fmt fmt-args data)))))

   ;; Note that `path' will most likely start with a slash
   (GET "/static:path{.*}" (path html)
        (catch
          'misc-error
          (lambda () (return
                 `((content-type ,(content-type html)))
                 (with-output-to-string
                   (lambda ()
                     ((sxml->output html)
                      (xhtml-doc
                       (head (title ,(_ "Calp directory listing for ") path)
                             ,(include-css
                               "/static/directory-listing.css"))
                       (body ,(directory-table (static-dir) path))))))))
          (lambda (err proc fmt fmt-args data)
            (return (build-response code: 404)
                    (format #f "~?" fmt fmt-args)))))

   ;; This is almost the same as /static/, but with the difference that
   ;; we produce these images during runtime
   (GET "/tmpfiles/:*{.*}.:ext" (* ext)
        ;; Actually parsing /etc/mime.types would be better.
        (define mime
          (case (string->symbol (string-downcase ext))
            [(png) "png"]
            [(jpg jpeg) "jpeg"]
            [(gif) "gif"]
            [else ext]))

        (return
         `((content-type ,(string->symbol (string-append "image/" mime))))
         ;; TODO handle tmp directory globaly
         (call-with-input-file (path-append (xdg-runtime-dir)
                                            "calp-data" "images"
                                            (string-append * "." ext))
           get-bytevector-all)))


   (GET "/count" ()
        ;; (sleep 1)
        (return '((content-type text/plain))
                (string-append (number->string state) "\n")
                (1+ state)))))

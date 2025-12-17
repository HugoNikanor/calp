(define-module (calp server routes)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (hnh util exceptions)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)

  :use-module ((ice-9 rdelim) :select (read-string))
  :use-module ((ice-9 ftw) :select (scandir))
  :use-module (ice-9 format)

  :use-module ((web response) :select (build-response))
  :use-module ((web uri) :select (build-relative-ref))
  :use-module ((web query) :select (encode-query-parameters))

  :use-module ((sxml simple) :select (sxml->xml xml->sxml))
  :use-module ((sxml html)   :select (sxml->html))
  :use-module (sxml xpath)
  :use-module (sxml namespace)

  :use-module ((rnrs io ports) :select (get-bytevector-all))
  :use-module ((xdg basedir) :prefix xdg-)

  :use-module ((base64) :select (base64decode))

  :use-module (web http make-routes)
  :use-module ((web query) :select (parse-query))
  :use-module ((ice-9 iconv) :select (bytevector->string))

  :use-module (vcomponent)
  :use-module (vcomponent util search)
  :use-module (datetime)

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
               (th ,(G_ "Name"))
               ;; File permissions, should be about as long as three digits
               (th ,(G_ "Perm"))
               ;; File size
               (th ,(G_ "Size"))))
    (tbody
     (tr (td "↩️") (td (@ (colspan 3))
                      (a (@ (href ,(-> (path-split dir)
                                       (drop-right 1)
                                       (xcons "/static")
                                       path-join)))
                         ,(G_ "Return up"))))
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
                       (G_ "Scandir argument invalid or not directory: ~s")
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
                     ;; TODO get objects
                     (stream)
                     ; (get-event-set global-event-object)
                     )))
            (hash-set! query-pages search-term q)
            q)))))



(define-config static-dir "static"
  description: (G_ "Where static files for the web server are located"))


(define ical-namespace '(IC . "urn:ietf:params:xml:ns:icalendar-2.0"))



(define (parse-urlencoded-body headers body)
  (cond ((assoc-ref headers 'content-type)
         => (lambda (content-type)
              (let ((type args (car+cdr content-type)))
                (when (eq? type 'application/x-www-form-urlencoded)
                  (let ((encoding (or (assoc-ref args 'encoding) "UTF-8")))
                    (parse-query (bytevector->string body encoding)
                                 encoding))))))
        (else '())))

;; TODO ensure encoding on all fields which take user provided data.
;; Possibly a fallback which strips everything unknown, and treats
;; the bytevector as ascii.
(define (make-make-routes)
  (define handler (make-handler))

  (set-request-start-log!
   handler
   (lambda* (key: method host port path (query "") allow-other-keys:)
     (display (format #f "[~a] ~a ~a:~a~a?~a~%"
                      (datetime->string (current-datetime))
                      ;; TODO the value we get for port is host a second time
                      method host port path query)
              (current-error-port))))

  (add-route!
   handler
   (make-route
    (GET "/" (html)
         (return (build-response code: 307
                                 headers: `((Location . "/today/")
                                            (content-type text/plain)))
                 (G_ "Redirecting to today, might take some time if server was just restarted.")))))

  (add-route!
   handler
   (make-route
    (GET "/favicon.ico" ()
         (return
          `((content-type image/svg+xml))
          (call-with-input-file "static/calendar.svg" read-string)))))

  ;; CAPUT
  (add-route!
   handler
   (make-route
    (GET "/everything.ics" (start end)
         (let ((start (or start (date- (current-date) (date day: 14))))
               (end (or end (date+ (current-date) (date year: 1)))))
           (let ((events
                  ;; TODO get events
                  '()
                  ;; (append
                  ;;  (fixed-events-in-range global-event-object start end)
                  ;;  (get-repeating-events global-event-object))
                  ))
             (format (current-error-port) "Collected ~a events~%" (length events))
             (return '((content-type text/calendar))
                     (with-output-to-string
                       (lambda () (print-components-with-fake-parent events)))))))))

  ;; TODO any exception in this causes the whole page to fail
  ;; It would be much better if most of the page could still make it.
  (add-route!
   handler
   (make-route
    (GET "/week/:start-date.html" (start-date html)
         (let ((start-date (start-of-week (parse-iso-date start-date))))
           (return `((content-type ,(content-type html)))
                   (with-output-to-string
                     (lambda ()
                       ((sxml->output html)
                        (html-generate calendars: ((@ (vcomponent config) data-stores))
                                       start-date: start-date
                                       end-date: (date+ start-date (date day: 6))
                                       next-start: (lambda (d) (date+ d (date day: 7)))
                                       prev-start: (lambda (d) (date- d (date day: 7)))
                                       render-calendar: (@ (calp html view calendar week) render-calendar)
                                       intervaltype: 'week)))))))))

  (add-route!
   handler
   (make-route
    (GET "/month/:start-date.html" (start-date html)
         (let ((start-date (start-of-month (parse-iso-date start-date))))
           (return `((content-type ,(content-type html)))
                   (with-output-to-string
                     (lambda ()
                       ((sxml->output html)
                        (html-generate calendars: ((@ (vcomponent config) data-stores))
                                       start-date: start-date
                                       end-date: (end-of-month start-date)
                                       next-start: (lambda (d) (date+ d (date month: 1)))
                                       prev-start: (lambda (d) (date- d (date month: 1)))
                                       render-calendar: (@ (calp html view calendar month)
                                                           render-calendar-table)
                                       pre-start: (start-of-week start-date)
                                       post-end: (end-of-week (end-of-month start-date))
                                       intervaltype: 'month
                                       )))))))))


  ;; Get specific page by query string instead of by path.
  ;; Useful for <form>'s, since they always submit in this form, but also
  ;; useful when javascript is disabled, since a link to "today" needs some
  ;; form of evaluation when clicked.
  (add-route!
   handler
   (make-route
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
                  headers: `((location . ,location)))))))


  ;; TODO BROKEN, see (vcomponent util search)
  (add-route!
   handler
   (make-route
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
                                  ;; TODO shouldn't q always be a string?
                                  (make-regexp ,(->string q)
                                               regexp/icase)
                                  (prop event 'SUMMARY)))))
                         ))))))))

  ;; TODO BROKEN, see (vcomponent util search)
  (add-route!
   handler
   (make-route
    (GET "/search" (q p onlyfuture html)
         (define search-term
           (if (and q (not (string-null? q)))
               (if onlyfuture
                   `(and (date/-time<=? ,(current-datetime) (prop1 event 'DTSTART))
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
                         path: r:path   ; host: r:host port: r:port
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

         (return `((content-type ,(content-type html)))
                 (lambda (port)
                   ((sxml->output html)
                    (search-result-page
                     error
                     (and=> q (negate string-null?))
                     search-term search-result page paginator)
                    port))))))

  ;; NOTE this only handles files with extensions. Limited, but since this
  ;; is mostly for development, and something like nginx should be used in
  ;; production it isn't a huge problem.


  (add-route!
   handler
   (make-route
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
                 (scm-error err proc fmt fmt-args data)))))))

  ;; Note that `path' will most likely start with a slash
  (add-route!
   handler
   (make-route
    (GET "/static:path{.*}" (path html)
         (catch
           'misc-error
           (lambda () (return
                  `((content-type ,(content-type html)))
                  (with-output-to-string
                    (lambda ()
                      ((sxml->output html)
                       (xhtml-doc
                        (head (title ,(G_ "Calp directory listing for ") path)
                              ,(include-css
                                "/static/directory-listing.css"))
                        (body ,(directory-table (static-dir) path))))))))
           (lambda (err proc fmt fmt-args data)
             (return (build-response code: 404)
                     (format #f "~?" fmt fmt-args)))))))


  (add-route!
   handler
   (make-route
    (GET "/count" ()
         ;; (sleep 1)
         (return '((content-type text/plain))
                 (string-append (number->string state) "\n")
                 (1+ state)))))

  ;; TODO these have been removed, and MUST be replaced by WebDAV resources

  ;; POST /remove
  ;; POST /insert
  ;; GET /calendar
  ;;   get a standalone calendar object, possibly limited by the
  ;;   parameters `start` and `end`.
  ;; GET /calendar/:uid{.*}.xcs
  ;; GET /calendar/:uid{.*}.ics

  ;; return
  (realize-handler handler))

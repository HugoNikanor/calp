(define-module (output html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcomponent)
  ;; #:use-module (vcomponent group)
  #:use-module (vcomponent datetime)
  #:use-module (util)
  #:use-module (util exceptions)
  #:use-module (util config)
  ;; #:use-module (util tree)
  #:duplicates (last)
  #:use-module (datetime)
  ;; #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (text util)
  #:use-module (vcomponent datetime output)

  #:use-module (html components)
  #:use-module (html util)
  #:use-module (html vcomponent)
  #:use-module ((html view calendar)
                :select (html-generate))

  #:use-module ((html view calendar week)
                :select (render-calendar))

  #:use-module ((html view calendar month)
                :select (render-calendar-table))

  #:autoload (vcomponent instance) (global-event-object)
  )



;; file existing but is of wrong type,
(define (create-files)
  (let* ((dir (dirname (or (@ (global) basedir) ".")))
         (html (string-append dir "/html"))
         (link (string-append html "/static")))
    (unless (file-exists? html)
      (mkdir html))
    (unless (file-exists? link)
      (symlink "../static" link))))

(define-public (html-chunked-main count start-date chunk-length)

  (define calendars (get-calendars global-event-object))
  (define events (get-event-set global-event-object))

  ((@ (util time) report-time!) "html start")

  (create-files)

  ;; NOTE Something here isn't thread safe.
  (stream-for-each
   (match-lambda
     [(start-date end-date)
      (let ((fname (format #f "~a/html/~a.html"
                           (dirname (or (@ (global) basedir) "."))
                           (date->string start-date "~1"))))
        (format (current-error-port) "Writing to [~a]~%" fname)
        (with-output-to-file fname
          (lambda () (html-generate calendars: calendars
                               events: events
                               start-date: start-date
                               end-date: end-date
                               render-calendar: render-calendar
                               next-start: (lambda (d) (date+ d chunk-length))
                               prev-start: (lambda (d) (date- d chunk-length))
                               ))))])
   (let ((ms (stream-iterate (cut date+ <> chunk-length) start-date)))
     (with-streams
      (take count
            (zip ms
                 (map (cut date- <> (date day: 1)) ; last in month
                      (cdr ms))))))))



(define-public (html-table-main count start-date)

  (define calendars (get-calendars global-event-object))
  (define events (get-event-set global-event-object))

  (create-files)

  (stream-for-each
   (lambda (start-of-month)
     (let ((fname (format #f "~a/html/~a.html"
                          (dirname (or (@ (global) basedir) "."))
                          (date->string start-of-month "~1"))))
       (format (current-error-port) "Writing to [~a]~%" fname)
       (let* ((before current after (month-days start-of-month (get-config 'week-start))))
         (with-output-to-file fname
           ;; TODO this produces incorrect next and prev links
           ;; TODO It actually produces almost all date links wrong
           (lambda () (html-generate calendars: calendars
                                events: events
                                ;; Appends for case where before or after is empty
                                start-date: (car current)
                                end-date: (date- (if (null? after)
                                                     (last current)
                                                     (car after))
                                                 (date day: 1))
                                render-calendar: render-calendar-table
                                next-start: month+
                                prev-start: month-
                                pre-start: (car (append before current))
                                post-end: (last (append current after))
                                ))))))
   (stream-take count (month-stream start-date))))

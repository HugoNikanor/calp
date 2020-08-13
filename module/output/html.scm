(define-module (output html)
  #:use-module (util)

  #:use-module ((srfi srfi-1) :select (last))
  #:use-module ((srfi srfi-41) :select (stream-take stream-for-each))
  #:use-module ((datetime)
                :select (date-stream date
                         remove-day month-days
                         date+ date-
                         month+ month-
                         date->string))
  #:use-module ((html view calendar)
                :select (html-generate))

  #:use-module ((html view calendar week)
                :select (render-calendar))

  #:use-module ((html view calendar month)
                :select (render-calendar-table))

  #:use-module ((vcomponent instance methods)
                :select (get-calendars get-event-set))

  #:use-module ((sxml simple) :select (sxml->xml))

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


(define (get-filename start-date)
  (format #f "~a/html/~a.html"
          (dirname (or (@ (global) basedir) "."))
          (date->string start-date "~1")))


(define (common count start-date chunk-length proc)

  (define calendars (get-calendars global-event-object))
  (define events (get-event-set global-event-object))

  ((@ (util time) report-time!) "html start")

  (create-files)

  (stream-for-each
   (lambda (start-date)
     (define fname (get-filename start-date))
     (format (current-error-port) "Writing to [~a]~%" fname)
     (with-output-to-file fname (lambda () (sxml->xml (proc calendars events))) ))
   (stream-take count (date-stream chunk-length start-date))
   ))


;; <int>, <date>, <date-duration> → xml string
(define-public (html-chunked-main count start-date chunk-length)

  (common count start-date chunk-length
          (lambda (calendars events)
            (html-generate calendars: calendars
                           events: events
                           start-date: start-date
                           end-date: (remove-day (date+ start-date chunk-length))
                           render-calendar: render-calendar
                           next-start: (lambda (d) (date+ d chunk-length))
                           prev-start: (lambda (d) (date- d chunk-length))
                           ))))



(define-public (html-table-main count start-date)

  (common count start-date (date month: 1)
          (let* ((before current after
                         (month-days start-date)))

            ;; TODO this produces incorrect next and prev links
            ;; TODO It actually produces almost all date links wrong
            (lambda (calendars events)
              (html-generate calendars: calendars
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
                             )))))

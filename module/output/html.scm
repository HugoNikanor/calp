(define-module (output html)
  #:use-module (util)

  #:use-module ((srfi srfi-1) :select (last))
  #:use-module ((srfi srfi-41) :select (stream-take stream-for-each))
  #:use-module ((datetime)
                :select (date-stream date
                         remove-day
                         date+ date-
                         date->string
                         start-of-week end-of-week
                         end-of-month
                         ))
  #:use-module ((html view calendar)
                :select (html-generate))

  #:use-module ((html view calendar week)
                :select (render-calendar))

  #:use-module ((html view calendar month)
                :select (render-calendar-table))

  #:use-module ((vcomponent instance methods)
                :select (get-calendars get-event-set))


  #:use-module ((ice-9 regex) :select (string-match regexp-substitute))

  #:use-module ((sxml simple) :select (sxml->xml))
  #:use-module ((sxml transformations) :select (href-transformer))

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
  (format #f "~a/html/~a.xml"
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
     (with-output-to-file fname
       (lambda () (sxml->xml
              (href-transformer
               (proc calendars events)
               (lambda (str)
                 (aif (string-match "^/static" str)
                      (regexp-substitute #f it 'pre "static" 'post)
                      str)))))))
   (stream-take count (date-stream chunk-length start-date))
   ))


;; <int>, <date>, <date-duration> → xml string
(define-public (html-chunked-main count start-date chunk-length)

  (common count start-date chunk-length
          (lambda (calendars events)
            (html-generate
             ;; same
             calendars: calendars
             events: events
             next-start: (lambda (d) (date+ d chunk-length))
             prev-start: (lambda (d) (date- d chunk-length))
             start-date: start-date
             end-date: (remove-day (date+ start-date chunk-length))
             render-calendar: render-calendar
             ;; different
             ))))

;; start date MUST be the first in month
(define-public (html-table-main count start-date)

  (define chunk-length (date month: 1))
  (define render-calendar render-calendar-table)

  (common count start-date chunk-length
          (lambda (calendars events)
            (html-generate
             ;; same
             calendars: calendars
             events: events
             next-start: (lambda (d) (date+ d chunk-length))
             prev-start: (lambda (d) (date- d chunk-length))
             start-date: start-date
             end-date: (remove-day (date+ start-date chunk-length))
             render-calendar: render-calendar

             ;; different
             pre-start: (start-of-week start-date)
             post-end: (end-of-week (end-of-month start-date))
             ))))

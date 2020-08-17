(define-module (calp entry-points html)
  :export (main)
  :use-module (util)
  :use-module (util time)
  :use-module (util options)
  :use-module (datetime)
  :use-module (ice-9 getopt-long)
  :use-module ((ice-9 regex) :select (string-match regexp-substitute))

  :use-module ((srfi srfi-41) :select (stream-take stream-for-each))
  :use-module ((html view calendar) :select (html-generate))
  :use-module ((html view calendar week)
               :select (render-calendar)
               :renamer (lambda _ 'render-calendar-wide))
  :use-module ((html view calendar month)
               :select (render-calendar-table))
  :use-module ((vcomponent instance methods)
                :select (get-calendars get-event-set))

  :use-module ((sxml simple) :select (sxml->xml))
  :use-module ((sxml transformations) :select (href-transformer))

  :autoload (vcomponent instance) (global-event-object)
  )


(define opt-spec
  `((from (value #t) (single-char #\F)
          (description "Start date of output.")
          )
    (count (value #t)
           (description "How many pages should be rendered."
                        "If --style=" (b "week") " and --from=" (b "2020-04-27")
                        " then --count=" (b 4) " would render the four pages "
                        "2020-04-27, 2020-05-04, 2020-05-11, and 2020-05-25. "
                        "Defaults to 12 to give a whole year when --style=" (b "month") "."
                        ))

    (style (value #t) (predicate ,(lambda (v) (memv (string->symbol v)
                                            '(wide week table))))
           (description "How the body of the HTML page should be layed out. "
                        (br) (b "week")
                        " gives a horizontally scrolling page with 7 elements, "
                        "where each has events graphically laid out hour by hour."
                        (br) (b "table")
                        " gives a month in overview as a table. Each block contains "
                        "the events for the given day, in order of start time. They are "
                        "however not graphically sized. "
                        (br) (b "wide")
                        " is the same as week, but gives a full month.")
           )

    (help (single-char #\h) (description "Print this help."))))



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

(define (re-root-static tree)
  (href-transformer
   tree
   (lambda (str)
     (aif (string-match "^/static" str)
          (regexp-substitute #f it 'pre "static" 'post)
          str))))

(define (common count start-date chunk-length
                render-calendar . extra-args)

  (define calendars (get-calendars global-event-object))
  (define events (get-event-set global-event-object))

  ((@ (util time) report-time!) "html start")

  (create-files)

  (stream-for-each
   (lambda (start-date)
     (define fname (get-filename start-date))
     (format (current-error-port) "Writing to [~a]~%" fname)
     (with-output-to-file fname
       (lambda () (sxml->xml (re-root-static
                         (apply html-generate
                                calendars: calendars
                                events: events
                                next-start: (lambda (d) (date+ d chunk-length))
                                prev-start: (lambda (d) (date- d chunk-length))
                                start-date: start-date
                                end-date: (remove-day (date+ start-date chunk-length))
                                render-calendar: render-calendar
                                extra-args))))))
   (stream-take count (date-stream chunk-length start-date))
   ))




(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))
  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define count (string->number (option-ref opts 'count "12")))

  (define style (string->symbol (option-ref opts 'style "wide")))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return)
    )

  ;; TODO a number of links are wrong, since they point to .html files,
  ;; while we save the documents as .xml.

  (case style
    [(wide)
     (common count start (date month: 1) render-calendar-wide)]

    [(week)

     ;; TODO The small calendar is always centered on months, it might
     ;; be a good idea to instead center it on the current week, meaning
     ;; that the active row is always in the center
     (common count (start-of-week start)
             (date day: 7)
             render-calendar-wide)]
    [(table)

     (common count (start-of-month start) (date month: 1)
             render-calendar-table
             pre-start: (start-of-week start)
             post-end: (end-of-week (end-of-month start)))]
    [else
     (error "Unknown html style: ~a" style)])

  ((@ (util time) report-time!) "all done")
  )

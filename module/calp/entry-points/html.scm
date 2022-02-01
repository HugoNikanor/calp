(define-module (calp entry-points html)
  :export (main)
  :use-module (hnh util)
  :use-module ((hnh util path) :select (path-append))
  :use-module (calp util time)
  :use-module (hnh util options)
  :use-module (datetime)
  :use-module (ice-9 getopt-long)
  :use-module ((ice-9 regex) :select (string-match regexp-substitute))

  :use-module ((srfi srfi-41) :select (stream-take stream-for-each))
  :use-module ((calp html view calendar) :select (html-generate))
  :use-module ((calp html view calendar week)
               :select (render-calendar)
               :renamer (lambda _ 'render-calendar-wide))
  :use-module ((calp html view calendar month)
               :select (render-calendar-table))
  :use-module ((vcomponent util instance methods)
                :select (get-calendars get-event-set))

  :use-module ((sxml simple) :select (sxml->xml))
  :use-module ((sxml transformations) :select (href-transformer))
  :use-module ((xdg basedir) :prefix xdg-)

  :autoload (vcomponent util instance) (global-event-object)
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

    (target (single-char #\t) (value #t)
            (description "Directory where html files should end up. Default to " (b "./html")))

    (style (value #t) (predicate ,(lambda (v) (memv (string->symbol v)
                                            '(small wide week table))))
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

    (standalone
     (description "Creates a standalone document instead of an HTML fragment "
                  "for embedding in a larger page. Currently only applies to the "
                  (i "small") "style"))

    (help (single-char #\h) (description "Print this help."))))



;; file existing but is of wrong type,
(define (create-files output-directory)

  (let* ((link (path-append output-directory "static")))

    (unless (file-exists? output-directory)
      (mkdir output-directory))

    ;; TODO nicer way to resolve static
    (let ((link (path-append output-directory "static")))
     (unless (file-exists? link)
       (symlink (path-append (xdg-data-home) "calp" "www" "static") link)))))


(define (re-root-static tree)
  (href-transformer
   tree
   (lambda (str)
     (aif (string-match "^/static" str)
          (regexp-substitute #f it 'pre "static" 'post)
          str))))

(define (common target-directory count start-date chunk-length
                render-calendar . extra-args)

  (define calendars (get-calendars global-event-object))
  (define events (get-event-set global-event-object))

  ((@ (calp util time) report-time!) "html start")

  (create-files target-directory)

  (stream-for-each
   (lambda (start-date)
     (define fname (path-append target-directory (date->string start-date "~1.xml")))
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

  (define target-directory (option-ref opts 'target "./html"))

  (define standalone (option-ref opts 'standalone #f))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return)
    )

  ;; TODO a number of links are wrong, since they point to .html files,
  ;; while we save the documents as .xml.

  (case style

    [(small)
     (let ((fname (path-append target-directory (date->string start "small-~1.xml"))))
       (with-output-to-file fname
         (lambda ()
           (sxml->xml
            (re-root-static
             ((@ (calp html view small-calendar) render-small-calendar)
              start standalone))))))]

    [(wide)
     (common target-directory count start (date month: 1) render-calendar-wide)]

    [(week)

     ;; TODO The small calendar is always centered on months, it might
     ;; be a good idea to instead center it on the current week, meaning
     ;; that the active row is always in the center
     (common target-directory count (start-of-week start)
             (date day: 7)
             render-calendar-wide)]
    [(table)

     (common target-directory
             count (start-of-month start) (date month: 1)
             render-calendar-table
             pre-start: (start-of-week start)
             post-end: (end-of-week (end-of-month start)))]
    [else
     (error "Unknown html style: ~a" style)])

  ((@ (calp util time) report-time!) "all done")
  )

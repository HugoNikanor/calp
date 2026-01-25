(define-module (calp entry-points html)
  :use-module (hnh util)
  :use-module ((hnh util exceptions) :select (warning))
  :use-module ((hnh util path) :select (path-append))
  :use-module (calp util time)
  :use-module (hnh util options)
  :use-module (datetime)
  :use-module (datetime io)
  :use-module (ice-9 getopt-long)
  :use-module ((ice-9 regex) :select (string-match regexp-substitute))
  :use-module (ice-9 format)

  :use-module ((srfi srfi-41) :select (stream stream-take stream-for-each))
  :use-module ((calp html view calendar) :select (html-generate))
  :use-module ((calp html view calendar week)
               :select (render-calendar)
               :renamer (lambda _ 'render-calendar-wide))
  :use-module ((calp html view calendar month)
               :select (render-calendar-table))
  :use-module ((calp html util) :select (html-file-extension))

  :use-module ((sxml simple) :select (sxml->xml xml->sxml))
  :use-module ((sxml transformations) :select (href-transformer))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module (calp translation)

  :export (main %summary)
  )

(define %summary
  (G_ "reads calendar files from disk, and writes them to static HTML files."))

(define opt-spec
  `((from (value #t) (single-char #\F)
          (description ,(G_ "Start date of output."))
          )
    (count (value #t)
           (description ,(xml->sxml (G_ "<group>How many pages should be rendered.
If --style=<b>week</b> and --from=<b>2020-04-27</b>;
then --count=<b>4</b> would render the four pages
2020-04-27, 2020-05-04, 2020-05-11, and 2020-05-25.
Defaults to 12 to give a whole year when --style=<b>month</b></group>"))))

    (target (single-char #\t) (value #t)
            (description ,(xml->sxml (G_ "<group>Directory where html files should end up. Default to <b>./html</b></group>"))))

    (style (value #t) (predicate ,(lambda (v) (memv (string->symbol v)
                                            '(small wide week table))))
           (description ,(xml->sxml (G_ "<group>How the body of the HTML page should be layed out.
<br/><b>week</b>
gives a horizontally scrolling page with 7 elements, where each has events
graphically laid out hour by hour.
<br/><b>table</b>
gives a month in overview as a table. Each block contains the events for the
given day, in order of start time. They are however not graphically sized.
<br/><b>wide</b>
is the same as week, but gives a full month.</group>"))))

    (standalone
     (description ,(xml->sxml (G_ "<group>Creates a standalone document instead of an HTML fragment
for embedding in a larger page. Currently only applies to the <i>small</i> style</group>"))))

    (tz
     (value #t)
     (description ,(G_ "Timezone to use as \"local time\" for generated files.")))

    (help (single-char #\h) (description ,(G_ "Print this help.")))))



;; file existing but is of wrong type,
(define (create-files output-directory)
  (define link (path-append output-directory "static"))
  ;; NOTE the target path is newer created
  (define target (path-append (xdg-data-home) "calp" "www" "static"))

  (unless (file-exists? output-directory)
    (mkdir output-directory))

  (catch 'system-error
    (lambda () (symlink target link))
    (lambda (err proc fmt fmt-args data)
      (define errno (car data))
      (cond ((= errno EACCES)
             (warning (format #f "~?" fmt fmt-args)))
            ((= errno EEXIST)
             (let ((st (lstat link)))
               (cond ((not (eq? 'symlink (stat:type st)))
                      (warning (G_ "File ~s exists, but isn't a symlink") link))
                     ((not (string=? target (readlink link)))
                      (warning (G_ "~s is a symlink, but points to ~s instead of expected ~s")
                               link (readlink link) target))))
             ;; else, file exists as a symlink, and points where we want,
             ;; which is expected. Do nothing and be happy.
             )
            ;; Rethrow
            (else (scm-error err proc fmt fmt-args data))))))


(define (re-root-static tree)
  (href-transformer
   tree
   (lambda (str)
     (aif (string-match "^/static" str)
          (regexp-substitute #f it 'pre "static" 'post)
          str))))

(define (common target-timezone
                target-directory count start-date chunk-length
                render-calendar . extra-args)

  ((@ (calp util time) report-time!) "html start")

  (create-files target-directory)

  (stream-for-each
   (lambda (start-date)
     (define fname (path-append target-directory (date->string start-date "~1.xml")))
     (format (current-error-port) (G_ "Writing to [~a]~%") fname)
     (with-output-to-file fname
       (lambda () (sxml->xml (re-root-static
                         (apply html-generate
                                calendars: ((@ (vcomponent config) data-stores))
                                next-start: (lambda (d) (date+ d chunk-length))
                                prev-start: (lambda (d) (date- d chunk-length))
                                start-date: start-date
                                end-date: (date- (date+ start-date chunk-length)
                                                 (date day: 1))
                                render-calendar: render-calendar
                                target-timezone: target-timezone
                                extra-args))))))
   (stream-take count (date-stream chunk-length start-date))
   ))




(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))
  (define start (cond [(option-ref opts 'from #f) => string->date]
                      ;; TODO default depends on style
                      ;; - month start for month
                      ;; - week start for week
                      [else (start-of-month (current-date))]))
  (define count (string->number (option-ref opts 'count "12")))

  (define style (string->symbol (option-ref opts 'style "wide")))

  (define target-directory (option-ref opts 'target "./html"))

  (define standalone (option-ref opts 'standalone #f))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return)
    )

  (define target-timezone
    (or (option-ref opts 'tz #f)
        (getenv "TZ")
        ((@ (datetime localtime) get-localtime))))

  (format (current-error-port) "start: ~s~%" start)

  (html-file-extension "xml")

  (case style

    [(small)
     (let ((fname (path-append target-directory (string-append (date->string start "small-~1.") (html-file-extension)))))
       (with-output-to-file fname
         (lambda ()
           (sxml->xml
            (re-root-static
             ((@ (calp html view small-calendar) render-small-calendar)
              start standalone))))))]

    [(wide)
     (common target-timezone target-directory count start (date month: 1) render-calendar-wide)]

    [(week)
     (common target-timezone target-directory count (start-of-week start)
             (date day: 7)
             render-calendar-wide)]

    [(table)
     (common target-timezone
             target-directory
             count (start-of-month start) (date month: 1)
             render-calendar-table)]

    [else
     (scm-error 'misc-error "html-main" (G_ "Unknown html style: ~a") (list style) #f)])

  ((@ (calp util time) report-time!) (G_ "all done"))
  )

(define-module (calp entry-points html)
  :export (main)
  :use-module (hnh util)
  :use-module ((hnh util exceptions) :select (warning))
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

  :use-module ((sxml simple) :select (sxml->xml xml->sxml))
  :use-module ((sxml transformations) :select (href-transformer))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module (calp translation)

  :autoload (vcomponent util instance) (global-event-object)
  )


(define opt-spec
  `((from (value #t) (single-char #\F)
          (description ,(_ "Start date of output."))
          )
    (count (value #t)
           (description ,(xml->sxml (_ "<group>How many pages should be rendered.
If --style=<b>week</b> and --from=<b>2020-04-27</b>;
then --count=<b>4</b> would render the four pages
2020-04-27, 2020-05-04, 2020-05-11, and 2020-05-25.
Defaults to 12 to give a whole year when --style=<b>month</b></group>"))))

    (target (single-char #\t) (value #t)
            (description ,(xml->sxml (_ "<group>Directory where html files should end up. Default to <b>./html</b></group>"))))

    (style (value #t) (predicate ,(lambda (v) (memv (string->symbol v)
                                            '(small wide week table))))
           (description ,(xml->sxml (_ "<group>How the body of the HTML page should be layed out.
<br/><b>week</b>
gives a horizontally scrolling page with 7 elements, where each has events
graphically laid out hour by hour.
<br/><b>table</b>
gives a month in overview as a table. Each block contains the events for the
given day, in order of start time. They are however not graphically sized.
<br/><b>wide</b>
is the same as week, but gives a full month.</group>"))))

    (standalone
     (description ,(xml->sxml (_ "<group>Creates a standalone document instead of an HTML fragment
for embedding in a larger page. Currently only applies to the <i>small</i> style</group>"))))

    (help (single-char #\h) (description ,(_ "Print this help.")))))



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
                      (warning (_ "File ~s exists, but isn't a symlink") link))
                     ((not (string=? target (readlink link)))
                      (warning (_ "~s is a symlink, but points to ~s instead of expected ~s")
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

(define (common target-directory count start-date chunk-length
                render-calendar . extra-args)

  (define calendars (get-calendars global-event-object))
  (define events (get-event-set global-event-object))

  ((@ (calp util time) report-time!) "html start")

  (create-files target-directory)

  (stream-for-each
   (lambda (start-date)
     (define fname (path-append target-directory (date->string start-date "~1.xml")))
     (format (current-error-port) (_ "Writing to [~a]~%") fname)
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
     (scm-error 'misc-error "html-main" (_ "Unknown html style: ~a") (list style) #f)])

  ((@ (calp util time) report-time!) (_ "all done"))
  )

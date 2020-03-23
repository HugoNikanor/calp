 (define-module (output html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcomponent)
  #:use-module (vcomponent group)
  #:use-module (vcomponent datetime)
  #:use-module (util)
  #:use-module (util tree)
  #:duplicates (last)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (output general)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)


  #:use-module (git)
  #:use-module (parameters)
  )


(define (date-link date)
  (date->string date "~Y-~m-~d"))

;; Generate an UID for an event
;; TODO currently not guaranteed to be unique
(define (UID ev)
  (string-append
   (datetime->string (as-datetime (attr ev 'DTSTART)) "~Y~m~d~H~M~S")
   (html-attr (attr ev 'UID))))

;; This should only be used on time intervals, never on absolute times.
;; For that see @var{date->decimal-hour}.
;; NOTE Above comment probably deprecated
(define (time->decimal-hour time)
  (exact->inexact (+ (hour time)
                     (/ (minute time) 60)
                     (/ (second time) 3600))))

;; Retuns an HTML-safe version of @var{str}.
(define (html-attr str)
  (define cs (char-set-adjoin char-set:letter+digit #\- #\_))
  (string-filter (lambda (c) (char-set-contains? cs c)) str))

;; Takes an event, and returns a pretty string for the time interval
;; the event occupies.
(define (fmt-time-span ev)
  (cond [(attr ev 'DTSTART) date?
         => (lambda (s)
              (cond [(attr ev 'DTEND)
                     => (lambda (e)
                          (if (date= e (date+ s (date day: 1)))
                              (date->string s)  ; start = end, only return one value
                              (values (date->string s)
                                      (date->string e))))]
                    ;; no end value, just return start
                    [else (date->string s)]))]
        [else ; guaranteed datetime
         (let ((s (attr ev 'DTSTART))
               (e (attr ev 'DTEND)))
           (let ((fmt-str (if (date= (get-date s) (get-date e))
                              "~H:~M" "~Y-~m-~d ~H:~M")))
             (values (datetime->string s fmt-str)
                     (datetime->string e fmt-str))))]))




;; Given a list, partitions it up into sublists of width length,
;;; each starting with 'tr.
(define (tablify list width)
  (unless (null? list)
    (let* ((row rest (split-at list width)))
      (cons `(tr ,@row)
            (tablify rest width)))))

;; An event is considered long if it's DTSTARt (and thereby DTEND) lacks a time component,
;; or if the total length of the event is greater than 24h.
;; For practical purposes, an event being long means that it shouldn't be rendered as a part
;; of a regular day.
(define (long-event? ev)
  (or (date? (attr ev 'DTSTART))
      (<= (* 3600 24)
       (datetime-difference (attr ev 'DTEND)
                            (attr ev 'DTSTART)))))


(define (event-debug-html event)
  `(table
    (tbody
     ,@(hash-map->list
        (match-lambda*
          [(key vline)
           `(tr (th ,key) (td ,(format #f "~a" (value vline))))]
          [_ (error "What are you doing‽")])
        (attributes event)))))



;;; Procedures for wide output


(define x-pos (make-object-property))
(define width (make-object-property))

;; Takes a list of vcomponents, sets their widths and x-positions to optimally
;; fill out the space, without any overlaps.
(define* (fix-event-widths! lst key: event-length-key (event-length-comperator date/-time>?))
  ;; The tree construction is greedy. This means
  ;; that if  a smaller  event preceeds a longer
  ;; event it would capture  the longer event to
  ;; only find  events which  also overlaps  the
  ;; smaller event.

  ;; @var{x} is how for left in the container we are.
  (let inner ((x 0)
              (tree (make-tree overlapping?
                               (sort* lst event-length-comperator event-length-key
                                      ))))
    (unless (null? tree)
      (let ((w (/ (- 1 x)
                  (+ 1 (length-of-longst-branch (left-subtree tree))))))
        (set! (width (car tree)) w
              (x-pos (car tree)) x)
        (inner (+ x w) (left-subtree tree))
        (inner x (right-subtree tree))))))


;; Format single event for graphical display
(define (create-block date ev)
  ;; (define time (date->time-utc day))
  (define style
    (format #f "left:~,3f%;width:~,3f%;top:~,3f%;height:~,3f%;"

            (* 100 (x-pos ev))          ; left
            (* 100 (width ev))          ; width

            ;; top
            (if (date= date (as-date (attr ev 'DTSTART)))
                (* 100/24
                   (time->decimal-hour
                    (as-time (attr ev 'DTSTART))))
                0)

            ;; height
            (* 100/24 (time->decimal-hour (event-length/day date ev)))))

  `(a (@ (href "#" ,(UID ev))
         (class "hidelink"))
      (div (@ (class "event CAL_" ,(html-attr (attr (parent ev) 'NAME))
                ,(when (and (attr ev 'PARTSTAT) (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                   " tentative")
                ,(when (date<? (as-date (attr ev 'DTSTART)) date)
                   " continued")
                ;; TODO all day events usually have the day after as DTEND.
                ;; So a whole day event the 6 june would have a DTEND of the
                ;; 7 june.
                ,(when (date<? date (as-date (attr ev 'DTEND)))
                   " continuing"))
              (style ,style))
           (div (@ (class "event-inner"))
                ;; NOTE These popup's are far from good. Main problem being that
                ;; the often render off-screen for events high up on the screen.
                (div (@ (class "popup"))
                     ,(event-debug-html ev))
                (div (@ (class "body"))
                     ,((summary-filter) ev (attr ev 'SUMMARY))))))

  )

(define (create-top-block start-date end-date ev)

  ;; NOTE be vary of api changes to date-diffenence
  (define total-length (* 24 (day (date-difference (date+ end-date (date day: 1)) start-date))))

  (define style
    (format #f "top:~,3f%;height:~,3f%;left:~,3f%;width:~,3f%;"

            ;; Prevent collisions
            (* 100 (x-pos ev))          ; top
            (* 100 (width ev))          ; height

            ;; Set start time
            ;; left
            (* 100
               (let ((dt (datetime date: start-date)))
                 (/ (datetime-difference (datetime-max dt (as-datetime (attr ev 'DTSTART))) dt)
                    3600 total-length)))

            ;; Set length of event, which makes end time
            ;; width
            (* 100
               (/ (event-length/clamped start-date end-date ev)
                  3600 total-length))))

  `(a (@ (href "#" ,(UID ev))
         (class "hidelink"))
      (div (@ (class "event CAL_" ,(html-attr (attr (parent ev) 'NAME))
                ,(when (and (attr ev 'PARTSTAT) (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                   " tentative")
                ,(when (date/-time< (attr ev 'DTSTART) start-date)
                   " continued")
                ,(when (date/-time< (date+ end-date (date day: 1)) (attr ev 'DTEND))
                   " continuing"
                   )
                )
              (style ,style))
           (div (@ (class "event-inner"))
                (div (@ (class "popup"))
                     ,(event-debug-html ev))
                (div (@ (class "body"))
                     ,((summary-filter) ev (attr ev 'SUMMARY)))))))


;; Lay out complete day (graphical)
;; (date . (events)) -> sxml
(define (lay-out-day day)
  (let* (((day-date . events) day)
         (time-obj (datetime date: day-date))
         (short-events (stream->list events)))

    (fix-event-widths! short-events event-length-key: (lambda (e) (event-length/day day-date e)))

    `(div (@ (class "events"))
          ,@(map (lambda (time)
                   `(div (@ (class "clock clock-" ,time)) ""))
                 (iota 12 0 2))
          ,@(map (lambda (e) (create-block day-date e)) short-events))))


(define (lay-out-long-events start end events)
  (fix-event-widths! events event-length-key: event-length
                     event-length-comperator: >)
  (map (lambda (e) (create-top-block start end e))
       events))

(define (time-marker-div)
  ;; element to make rest of grid align correct.
  ;; Could be extended to contain something fun.
  `((div (@ (style "grid-row: 1 / span 2")) "")
    (div (@ (class "sideclock"))
         ,@(map (lambda (time)
                  `(div (@ (class "clock clock-" ,time))
                        (span (@ (class "clocktext"))
                              ,time ":00")))
                (iota 12 0 2)))))

;; date, date, [sorted-stream events] → [list events]
(define (events-between start-date end-date events)
  (define (overlaps e)
    (timespan-overlaps? start-date (date+ end-date (date day: 1))
                        (attr e 'DTSTART) (attr e 'DTEND)))

  (stream-filter overlaps
                 (get-stream-interval
                  overlaps
                  (lambda (e) (not (date< end-date (as-date (attr e 'DTSTART)))))
                  events)))

;; Returns number of days in time interval.
;; @example
;; (days-between #2020-01-01 #2020-01-05)
;; ⇒ 5
;; @end example
;; date, date → int
(define (days-between start-date end-date)
  ;; TODO be vary if date-difference changes again
  (1+ (day (date-difference end-date start-date))))

;; date, date → [list date]
(define (date-range start end)
  (stream->list
   (stream-take-while (lambda (d) (date<= d end))
                      (day-stream start))))

(define*-public (render-calendar key: events start-date end-date #:allow-other-keys)
  (let* ((long-events short-events (partition long-event? (stream->list (events-between start-date end-date events))))
         (range (date-range start-date end-date)))
    `(div (@ (class "calendar"))
          (div (@ (class "days"))
               ,@(time-marker-div)
               (div (@ (class "longevents")
                       (style "grid-column-end: span " ,(days-between start-date end-date)))
                    ""                  ; prevent self-closing
                    ,@(lay-out-long-events start-date end-date long-events))
               ,@(map (lambda (day-date)
                        `(div (@ (class "meta"))
                              ,(let ((str (date-link day-date)))
                                 `(span (@ (id ,str) (class "daydate")) ,str))
                              (span (@ (class "dayname")) ,(date->string day-date "~a"))) )
                      range)
               ,@(stream->list
                     (stream-map
                      lay-out-day
                      (get-groups-between (group-stream (list->stream short-events))
                                          start-date end-date)))))))

  
;;; Prodcedures for text output

;; For sidebar, just text
(define (fmt-single-event ev)
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (attr ev 'X-HNH-FILENAME))
  `(article (@ (id ,(UID ev))
               (class "eventtext CAL_bg_"
                 ,(html-attr (attr (parent ev) 'NAME))
                 ,(when (and (attr ev 'PARTSTAT) (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                    " tentative")))
            (h3 (a (@ (href "#" ,(date-link (as-date (attr ev 'DTSTART))))
                      (class "hidelink"))
                   ,(attr ev 'SUMMARY)))
            (div
             ,(call-with-values (lambda () (fmt-time-span ev))
                (match-lambda* [(start end) `(div ,start " — " ,end)]
                               [(start) `(div ,start)]))
             ,(when (and=> (attr ev 'LOCATION) (negate string-null?))
                `(div (b "Plats: ") ,(attr ev 'LOCATION)))
             ,(and=> (attr ev 'DESCRIPTION) (lambda (str) ((description-filter) ev str))))))

;; Single event in side bar (text objects)
(define (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map fmt-single-event
                             (stream-filter
                              (lambda (ev)
                                ;; If start was an earlier day
                                ;; This removes all descriptions from
                                ;; events for previous days,
                                ;; solving duplicates.
                                (date/-time<=? date (attr ev 'DTSTART)))
                              events))))))


;;; Table output

(define (make-small-block event)
  `(a (@ (href "#" ,(UID event))
         (class "hidelink"))
      (div (@ (class "inline-event CAL_"
                ,(html-attr (attr (parent event) 'NAME))))
           ,((summary-filter) event (attr event 'SUMMARY)))))

;; (stream event-group) -> sxml
(define* (render-calendar-table key: events start-date end-date pre-start post-end #:allow-other-keys)

  (define event-groups (get-groups-between (group-stream events)
                                           pre-start post-end))

  `(div (@ (class "caltable"))
        ,@(map (lambda (d) `(div (@ (class "thead")) ,(week-day-name d)))
               (weekday-list (week-start)))
        ,@(cons
           ;; First day is a special case, since I always want to show a full date there.
           ;; For all other days I'm only interested in the parts that change.
           (let* (((day-date . events) (stream-car event-groups)))
             `(div (@ (class "cal-cell"))
                   (div (@ (class "date-info"))
                        (span (@ (class "day-number")) ,(date->string day-date "~e"))
                        (span (@ (class "month-name")) ,(date->string day-date "~b"))
                        (span (@ (class "year-number")) ", " ,(date->string day-date "~Y")))
                   ,@(stream->list (stream-map make-small-block events))))
           (stream->list
            (stream-map
             (match-lambda
               [(day-date . events)
                `(div (@ (class "cal-cell"))
                      (div (@ (class "date-info "
                                ,(when (or (date< day-date start-date)
                                           (date< end-date day-date))
                                   "non-current")
                                ))
                           (span (@ (class "day-number")) ,(date->string day-date "~e"))
                           ,(when (= 1 (day day-date))
                              `(span (@ (class "month-name")) ,(date->string day-date "~b")))
                           ,(when (= 1 (month day-date) (day day-date))
                              `(span (@ (class "year-number"))
                                     ", " ,(date->string day-date "~Y"))))
                      ,@(stream->list
                         (stream-map make-small-block events)))])
             (stream-cdr event-groups))))))



;;; General HTML help

(define (include-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,path)
            ,@extra-attributes)))

(define (include-alt-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "alternate stylesheet")
            (href ,path)
            ,@extra-attributes)))



;; date should be start of month
;; @example
;; må ti on to fr lö sö
;;  1  2  3  4  5  6  7
;;  8  9 10 11 12 13 14
;; 15 16 17 18 19 20 21
;; 22 23 24 25 26 27 28
;; 29 30
;; @end example
;; date - a date in the month to display
;; week-start - which day the week begins on, see (datetime util)
(define* (cal-table key:
                    start-date
                    end-date
                    (week-start (week-start))
                    next-start
                    prev-start)
  (define (td date)
    `(td (@ (class
              ,(when (date< date start-date) "prev ")
              ,(when (date< end-date date) "next ")))
         (a (@ (href ,(cond [(date< date start-date)
                             ;; TODO find a prettier way to generate links to previous and next time intervals
                             ;; TODO also, it would do good with a bit of testing for off-by-one errors
                             (date->string
                              (stream-find (lambda (d) (date<= d date (next-start d)))
                                           (stream-iterate prev-start start-date))
                              "~Y-~m-~d.html")]
                            [(date< end-date date)
                             (date->string
                              (stream-find (lambda (d) (and (date<= d date)
                                                       (date< date (next-start d))))
                                           (stream-iterate next-start start-date))
                              "~Y-~m-~d.html" )])
                     "#" ,(date->string date "~Y-~m-~d"))
               (class "hidelink"))
            ,(day date))))

  `(table (@ (class "small-calendar"))
          (thead (tr ,@(map (lambda (d) `(td ,(week-day-name d 2)))
                            (weekday-list week-start))))

          ((tbody ,@(let* ((last current next
                                 ;; A calendar table is always exactly one month, therefore
                                 ;; it's safe to say that we are interested in the month which
                                 ;; start-date is part of
                                 (month-days (start-of-month start-date) week-start)))
                      (tablify (map td (append last current next))
                               7))))))



;;; Main-stuff


;;; NOTE
;;; The side bar filters all earlier events for each day to not create repeats,
;;; and the html-generate procedure also filters, but instead to find earlier eventns.
;;; All this filtering is probably slow, and should be looked into.

(define repo-url (make-parameter "https://git.hornquist.se"))

(define*-public (html-generate calendars events start-date end-date
                               render-calendar ; (bunch of kv args) → sxml
                               key:
                               next-start ; date → date
                               prev-start ; date → date
                               ;; The pre and post dates are if we want to show some dates just outside our
                               ;; actuall interval. Primarily for whole month views, which needs a bit on each side.
                               (pre-start start-date)
                               (post-end end-date)
                               )
  ;; TODO maybe don't do this again for every month
  (define evs (get-groups-between (group-stream events)
                                  start-date end-date))

  (define (nav-link display date)
    `(a (@ (href ,(date->string date "~Y-~m-~d") ".html")
           (class "nav hidelink"))
        (div (@ (class "nav"))
             ,display)))

  (unless next-start
    (error 'html-generate "Next-start needs to be a procedure"))

  (unless prev-start
    (error 'html-generate "Prev-start needs to be a procedure"))

  (display "<!doctype HTML>\n")
  ((@ (sxml simple) sxml->xml)
   `(html (@ (lang sv))
          (head
           (title "Calendar")
           (meta (@ (charset "utf-8")))
           (meta (@ (http-equiv "Content-Type") (content "application/xhtml+xml")))
           (meta (@ (name viewport)
                    (content "width=device-width, initial-scale=0.5")))
           (meta (@ (name description)
                    (content "Calendar for the dates between " ,(date->string start-date)
                             " and " ,(date->string end-date))))
           ,(include-css "static/style.css")
           ,(include-alt-css "static/dark.css"  '(title "Dark"))
           ,(include-alt-css "static/light.css" '(title "Light"))
           (script (@ (src "static/script.js")) "")
           (style ,(format #f "~:{.CAL_~a { background-color: ~a; color: ~a }~%.CAL_bg_~a { border-color: ~a }~%~}"
                           (map (lambda (c)
                                  (let* ((name (html-attr (attr c 'NAME)))
                                         (bg-color (attr c 'COLOR))
                                         (fg-color (and=> (attr c 'COLOR)
                                                          calculate-fg-color)))
                                    (list name (or bg-color 'white) (or fg-color 'black)
                                          name (or bg-color 'black))))
                                calendars))))

          (body
           (div (@ (class "root"))
                (main
                 ;; Actuall calendar
                 ,(render-calendar calendars: calendars
                                   events: events
                                   start-date: start-date
                                   end-date: end-date
                                   pre-start: pre-start
                                   post-end: post-end
                                   next-start: next-start
                                   prev-start: prev-start
                                   )

                 ;; Page footer
                 (footer (span "Page generated " ,(date->string (current-date)))
                         (span (a (@ (href ,(repo-url) "/calparse"))
                                  "Source Code"))
                         ,(let* ((hash (get-git-version))
                                 (url (format #f "~a/calparse/commit/?id=~a"
                                              (repo-url) hash)))
                            `(span "Version " (a (@ (href ,url)) ,hash)))))

                ;; Whole sidebar
                (aside (@ (class "sideinfo"))
                       ;; Small calendar and navigation
                       (div (@ (class "about"))
                            ;; prev button
                            ,(nav-link "«" (prev-start start-date))

                            ;; calendar table
                            ;; TODO
                            (div ,(cal-table start-date: start-date end-date: end-date
                                             next-start: next-start
                                             prev-start: prev-start
                                             ))

                            ;; next button
                            ,(nav-link "»" (next-start start-date)))


                       ;; List of events
                       (div (@ (class "eventlist"))
                            ;; List of calendars
                            (div (@ (class "calendarlist"))
                                 (ul ,@(map (lambda (calendar)
                                              `(li (@ (class "CAL_bg_" ,(html-attr (attr calendar 'NAME))))
                                                   ,(attr calendar 'NAME)))
                                            calendars)))

                            ;; Events which started before our start point, but "spill" into our time span.
                            (section (@ (class "text-day"))
                                     (header (h2 "Tidigare"))
                                     ,@(stream->list
                                        (stream-map fmt-single-event
                                                    (stream-take-while (compose (cut date/-time<? <> start-date)
                                                                                (extract 'DTSTART))
                                                                       (cdr (stream-car evs))))))
                            ,@(stream->list (stream-map fmt-day evs)))))))))


(define-public (html-chunked-main count calendars events start-date chunk-length)
  ;; TODO This still doesn't account for PWD, file existing but is of
  ;; wrong type, html directory existing but static symlink missing,
  ;; static being a different file type, and probably something else
  ;; i'm missing.
  (unless (file-exists? "./html")
    (mkdir "./html")
    (symlink "../static" "./html/static"))

  ;; NOTE Something here isn't thread safe.
  ;; TODO make it thread safe
  (stream-for-each
   (match-lambda
     [(start-date end-date)
      (let ((fname (format #f "./html/~a.html" (date->string start-date "~1"))))
        (format (current-error-port) "Writing to [~a]~%" fname)
        (with-output-to-file fname
          (lambda () (html-generate calendars events start-date end-date render-calendar
                               next-start: (lambda (d) (date+ d chunk-length))
                               prev-start: (lambda (d) (date- d chunk-length))
                               ))))])
   (let ((ms (stream-iterate (cut date+ <> chunk-length) start-date)))
     (with-streams
      (take count
            (zip ms
                 (map (cut date- <> (date day: 1)) ; last in month
                      (cdr ms))))))))


(define-public (html-table-main count calendars events start-date)
  ;; TODO same file creation as in html-chunked-main
  (stream-for-each
   (lambda (start-of-month)
     (let ((fname (format #f "./html/~a.html" (date->string start-of-month "~1"))))
       (format (current-error-port) "Writing to [~a]~%" fname)
       (let* ((before current after (month-days start-of-month (week-start))))
         (with-output-to-file fname
           ;; TODO this produces incorrect next and prev links
           ;; TODO It actually produces almost all date links wrong
           (lambda () (html-generate calendars events
                                ;; Appends for case where before or after is empty
                                (car current) (date- (if (null? after)
                                                         (last current)
                                                         (car after))
                                                     (date day: 1))
                                render-calendar-table
                                next-start: month+
                                prev-start: month-
                                pre-start: (car (append before current))
                                post-end: (last (append current after))
                                ))))))
   (stream-take count (month-stream start-date))))

(define-module (output html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcomponent)
  #:use-module (vcomponent group)
  #:use-module (vcomponent datetime)
  #:use-module (util)
  #:use-module (util app)
  #:use-module (util exceptions)
  #:use-module (util config)
  #:use-module (util tree)
  #:duplicates (last)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (output general)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (output text)


  #:use-module (git)
  ;; #:use-module (module config all)
  )

(define-config summary-filter (lambda (_ a) a)
  ""
  procedure?)

(define-config description-filter (lambda (_ a) a)
  ""
  procedure?)


(define* (slider-input key: variable
                       (min 0)
                       (max 10)
                       (step 1)
                       (value 1)
                       (unit ""))
  (let ((groupname (symbol->string (gensym "slider"))))
    `(div (@ (class "input-group"))
          (script
           "function " ,groupname "fn (value) {"
           "setVar('" ,variable "', value + '" ,unit "');"
           "for (let el of document.getElementsByClassName('" ,groupname "')) {"
           "    el.value = value;"
           "}}")
          (input (@ (type "range")
                    (class ,groupname)
                    (min ,min)
                    (max ,max)
                    (step ,step)
                    (value ,value)
                    (oninput ,groupname "fn(this.value)")
                    ))
          (input (@ (type "number")
                    (class ,groupname)
                    (min ,min)
                    (max ,max)
                    (step ,step)
                    (value ,value)
                    (oninput ,groupname "fn(this.value)"))
                 )
          )))

(define (date-link date)
  (date->string date "~Y-~m-~d"))

;; Generate an UID for an event
;; TODO currently not guaranteed to be unique
(define (UID ev)
  (string-append
   (datetime->string (as-datetime (attr ev 'DTSTART)) "~Y~m~d~H~M~S")
   (html-attr (attr ev 'UID))))

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



(define* (btn key: onclick href (class '())
              allow-other-keys:
              rest: args)
  (when (and onclick href)
    (error "Only give one of onclick, href and submit."))

  (let ((body #f))
    `(,(cond [href 'a]
             [else 'button])
      (@ (class ,(string-join (cons "btn" class) " "))
         ,@(cond [onclick `((onclick ,onclick))]
                 [href `((href ,href))]
                 [else '()])
         ,@(let loop ((rem args))
             (cond
              [(null? rem) '()]
              [(keyword? (car rem))
               (cons* `(,(keyword->symbol (car rem))
                        ,(cadr rem))
                      (loop (cddr rem)))]
              [else
               (set! body (car rem))
               (loop (cdr rem))])))
      (div ,body))))


(define (popup ev id)
  `(div (@ (class "popup-container") (id ,id))
        (div (@ (class "popup"))
             (nav (@ (class "popup-control CAL_" ,(html-attr (or (attr (parent ev) 'NAME)
                                                                 "unknown"))))
                  ,(btn "×"
                        title: "Stäng"
                        onclick: ""
                        class: '("close-tooltip")
                        )
                  ,(btn "📅"
                        title: "Ladda ner"
                        href: (string-append "/calendar/" (attr ev 'UID) ".ics")))

             ,(fmt-single-event ev))))

(define (data-attributes event)
  (hash-map->list
   (match-lambda*
     [(key vline)
      (list (string->symbol (format #f "data-~a" key))
            (format #f "~a" (value vline)))]
     [_ (error "What are you doing‽")])
   (attributes event)))



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

(define* (make-block ev optional: (extra-attributes '()))

  (define popup-id (symbol->string (gensym "popup")))

  `((a (@ (href "#" ,(UID ev))
          (class "hidelink"))
       (div (@ ,@(assq-merge
                  extra-attributes
                  `((class "event CAL_" ,(html-attr (or (attr (parent ev) 'NAME)
                                                        "unknown"))
                      ,(when (and (attr ev 'PARTSTAT)
                                  (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                         " tentative"))
                    (data-tipped-options ,(format #f "inline: '~a'" popup-id))
                    ;; TODO only if in debug mode?
                    ,@(data-attributes ev))))
            ,(when (attr ev 'RRULE)
               `(span (@ (class "repeating")) "↺"))
            ,((get-config 'summary-filter) ev (attr ev 'SUMMARY))
            ,(when (attr ev 'LOCATION)
               `(span (@ (class "location"))
                      ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                   (attr ev 'LOCATION))))))
    ,(popup ev popup-id)))

;; Format single event for graphical display
(define (create-block date ev)
  ;; (define time (date->time-utc day))
  (define style
    ;; The calc's here is to enable an "edit-mode".
    ;; Setting --editmode ≈ 0.8 gives some whitespace to the right
    ;; of the events, alowing draging there for creating new events.
    ;; TODO only include var and calc when editing should be enabled.
    (format #f "left:calc(var(--editmode)*~,3f%);width:calc(var(--editmode)*~,3f%);top:~,3f%;height:~,3f%;"

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

  (make-block
   ev `((class
          ,(when (date<? (as-date (get-datetime (attr ev 'DTSTART))) date)
             " continued")
          ;; TODO all day events usually have the day after as DTEND.
          ;; So a whole day event the 6 june would have a DTEND of the
          ;; 7 june.
          ,(when (date<? date (as-date (get-datetime (attr ev 'DTEND))))
             " continuing"))
        (style ,style))))

;; date{,time}-difference works in days, and days are simply multiplied by 24 to
;; get hours.  This means that a day is always assumed to be 24h, even when that's
;; wrong. This might lead to some weirdness when the timezon switches (DST), but it
;; makes everything else behave MUCH better.
(define (create-top-block start-date end-date ev)

  (define total-length
    (* 24 (days-in-interval start-date end-date)))

  (define style
    (format #f "top:~,3f%;height:~,3f%;left:~,3f%;width:~,3f%;"

            ;; Prevent collisions
            (* 100 (x-pos ev))          ; top
            (* 100 (width ev))          ; height

            ;; Set start time
            ;; left
            (* 100
               (let* ((dt (datetime date: start-date))
                      (diff (datetime-difference
                             (datetime-max dt (as-datetime (attr ev 'DTSTART)))
                             dt)))
                 (/ (datetime->decimal-hour diff start-date) total-length)))

            ;; Set length of event, which makes end time
            ;; width
            (* 100
               (/ (datetime->decimal-hour
                   (as-datetime (event-length/clamped start-date end-date ev))
                   start-date)
                  total-length))))

  (make-block
   ev `((class
          ,(when (date/-time< (attr ev 'DTSTART) start-date)
             " continued")
          ,(when (date/-time< (date+ end-date (date day: 1)) (attr ev 'DTEND))
             " continuing"))
        (style ,style))))


;; Lay out complete day (graphical)
;; (date . (events)) -> sxml
(define (lay-out-day day)
  (let* (((day-date . events) day)
         (time-obj (datetime date: day-date))
         (short-events (stream->list events)))

    (fix-event-widths! short-events event-length-key:
                       (lambda (e) (event-length/day day-date e)))

    `(div (@ (class "events") (id ,(date-link day-date)))
          ,@(map (lambda (time)
                   `(div (@ (class "clock clock-" ,time)) ""))
                 (iota 12 0 2))
          ,@(map (lambda (e) (create-block day-date e)) short-events))))


(define (lay-out-long-events start end events)
  (fix-event-widths! events event-length-key: event-length
                     event-length-comperator: date/-time>)
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


(define*-public (render-calendar key: events start-date end-date #:allow-other-keys)
  (let* ((long-events short-events (partition long-event? (stream->list (events-between start-date end-date events))))
         (range (date-range start-date end-date)))
    `((div (@ (class "calendar"))
           (div (@ (class "days"))
                ,@(time-marker-div)
                (div (@ (class "longevents")
                        (style "grid-column-end: span " ,(days-in-interval start-date end-date)))
                     ""                 ; prevent self-closing
                     ,@(lay-out-long-events start-date end-date long-events))
                ,@(map (lambda (day-date)
                         `(div (@ (class "meta"))
                               (span (@ (class "daydate"))
                                     ,(date->string day-date "~Y-~m-~d"))
                               (span (@ (class "dayname"))
                                     ,(string-titlecase (date->string day-date "~a")))))
                       range)
                ,@(stream->list
                   (stream-map
                    lay-out-day
                    (get-groups-between (group-stream (list->stream short-events))
                                        start-date end-date))))))))


;;; Prodcedures for text output

;; ev → sxml
(define (format-recurrence-rule ev)
  `(span (@ (class "rrule"))
         "Upprepas "
         ,((compose (@ (vcomponent recurrence display) format-recurrence-rule)
                    (@ (vcomponent recurrence parse) parse-recurrence-rule))
           (attr ev 'RRULE))
         ,@(awhen (attr ev 'EXDATE)
                  (list
                   ", undantaget "
                   (add-enumeration-punctuation
                    (map (lambda (d)
                           (if (date? d)
                               ;; TODO show year?
                               (date->string d "~e ~b")
                               ;; NOTE only show time when it's different than the start time?
                               ;; or possibly only when FREQ is hourly or lower.
                               (if (memv ((@ (vcomponent recurrence internal) freq)
                                       ((@ (vcomponent recurrence parse)
                                           parse-recurrence-rule)
                                        (attr ev 'RRULE)))
                                      '(HOURLY MINUTELY SECONDLY))
                                   (datetime->string d "~e ~b ~k:~M")
                                   (datetime->string d "~e ~b"))))
                         it))))
         "."))


;; For sidebar, just text
(define* (fmt-single-event ev
                           optional: (attributes '())
                           key: (fmt-header list))
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (attr ev 'X-HNH-FILENAME))
  `(article (@ ,@(assq-merge
                  attributes
                  `((class "eventtext CAL_bg_"
                      ,(html-attr (or (attr (parent ev) 'NAME) "unknown"))
                      ,(when (and (attr ev 'PARTSTAT)
                                  (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                         " tentative")))))
            (h3 ,(fmt-header
                  (when (attr ev 'RRULE)
                    `(span (@ (class "repeating")) "↺"))
                  (attr ev 'SUMMARY)))
            (div
             ,(call-with-values (lambda () (fmt-time-span ev))
                (match-lambda* [(start end) `(div ,start " — " ,end)]
                               [(start) `(div ,start)]))
             ,(when (and=> (attr ev 'LOCATION) (negate string-null?))
                `(div (b "Plats: ")
                      (div (@ (class "location"))
                           ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                        (attr ev 'LOCATION)))))
             ,(and=> (attr ev 'DESCRIPTION)
                     (lambda (str) (catch #t (lambda () ((get-config 'description-filter) ev str))
                                (lambda (err . args)
                                  (warning "~a on formatting description, ~s" err args)
                                  str))))
             ,(awhen (attr ev 'RRULE)
                     (format-recurrence-rule ev))
             ,(when (attr ev 'LAST-MODIFIED)
                `(span (@ (class "last-modified")) "Senast ändrad "
                       ,(datetime->string (attr ev 'LAST-MODIFIED) "~1 ~H:~M")))

             )))

;; Single event in side bar (text objects)
(define (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map
                  (lambda (ev) (fmt-single-event
                           ev `((id ,(UID ev)))
                           fmt-header:
                           (lambda body
                             `(a (@ (href "#" ,(date-link (as-date (attr ev 'DTSTART))))
                                    (class "hidelink"))
                                 ,@body))))
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
  (make-block event))

;; (stream event-group) -> sxml
(define*-public (render-calendar-table key: events start-date end-date pre-start post-end #:allow-other-keys)

  (define event-groups (get-groups-between (group-stream events)
                                           pre-start post-end))

  `((header (@ (class "table-head"))
            ,(string-titlecase (date->string start-date "~B ~Y")))
    (div (@ (class "caltable"))
         ,@(map (lambda (d) `(div (@ (class "thead")) ,(string-titlecase (week-day-name d))))
                (weekday-list (get-config 'week-start)))
         ,@(cons
            ;; First day is a special case, since I always want to show a full date there.
            ;; For all other days I'm only interested in the parts that change.
            (let* (((day-date . events) (stream-car event-groups)))
              `(div (@ (class "cal-cell"))
                    (time (@ (class "date-info")
                             (datetime ,(date->string day-date "~1")))
                          (span (@ (class "day-number")) ,(date->string day-date "~e"))
                          (span (@ (class "month-name")) ,(date->string day-date "~b"))
                          (span (@ (class "year-number"))
                                ", " ,(date->string day-date "~Y")))
                    ,@(stream->list (stream-map make-small-block events))))
            (stream->list
             (stream-map
              (match-lambda
                [(day-date . events)
                 `(div (@ (class "cal-cell"))
                       (time (@ (class "date-info "
                                  ,(if (or (date< day-date start-date)
                                           (date< end-date day-date))
                                       "non-current"
                                       "current"))
                                (datetime ,(date->string day-date "~1")))
                             (span (@ (class "day-number"))
                                   ,(date->string day-date "~e"))
                             ,(when (= 1 (day day-date))
                                `(span (@ (class "month-name"))
                                       ,(date->string day-date "~b")))
                             ,(when (= 1 (month day-date) (day day-date))
                                `(span (@ (class "year-number"))
                                       ", " ,(date->string day-date "~Y"))))
                       ,@(stream->list (stream-map make-small-block events)))])
              (stream-cdr event-groups)))))))



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
(define* (cal-table key: start-date end-date next-start prev-start
                    (week-start (get-config 'week-start)))

  (define (td date)
    `(a (@ ,@(cond
              ;; We are before our time interval
              [(date< date start-date)
               ;; TODO find a prettier way to generate links to previous and next time intervals
               ;; TODO also, it would do good with a bit of testing for off-by-one errors
               `((class "prev")
                 (href ,(date->string
                         (stream-find (lambda (d) (date<= d date (next-start d)))
                                      (stream-iterate prev-start start-date))
                         "~Y-~m-~d.html")
                       "#" ,(date-link date)))]
              ;; We are after our time interval
              [(date< end-date date)
               `((class "next")
                 (href ,(date->string
                         (stream-find (lambda (d) (and (date<= d date)
                                                  (date< date (next-start d))))
                                      (stream-iterate next-start start-date))
                         "~Y-~m-~d.html")
                       "#" ,(date-link date)))]
              ;; We are in our time interval
              [else `((href "#" ,(date-link date)))]))
        ;; NOTE This time object is the correct place to show the existance
        ;; of an event on a given day in this small calendar. For example
        ;; making the text red for all holidays, or creating a yellow background
        ;; for events from a specific source.
        (time (@ (datetime ,(date->string date "~Y-~m-~d"))) ,(day date))))

  (let* ((last-months current next
                      (month-days (start-of-month start-date) week-start))
         (events (append last-months current next)))
    `(div (@ (class "small-calendar"))
          (div (@ (class "column-head row-head")) "v.")
          ,@(map (lambda (d) `(div (@ (class "column-head"))
                              ,(string-titlecase (week-day-name d 2))))
                 (weekday-list week-start))
          ,@(let ((first (week-number (car events) week-start))
                  (last (week-number (last events) week-start)))
              (map (lambda (v) `(div (@ (class "row-head")) ,v))
                   ;; TODO this fails around new-year
                   (iota (1+ (- last first)) first)))
          ,@(map td events
                 ))))



;;; Main-stuff


;;; NOTE
;;; The side bar filters all earlier events for each day to not create repeats,
;;; and the html-generate procedure also filters, but instead to find earlier eventns.
;;; All this filtering is probably slow, and should be looked into.

(define repo-url (make-parameter "https://git.hornquist.se"))

(define*-public (html-generate
                 key:
                 (intervaltype 'all)    ; 'week | 'month | 'all
                 calendars events start-date end-date
                 render-calendar        ; (bunch of kv args) → (list sxml)
                 next-start             ; date → date
                 prev-start             ; date → date
                 ;; The pre and post dates are if we want to show some dates just
                 ;; outside our actuall interval. Primarily for whole month views,
                 ;; which needs a bit on each side.
                 (pre-start start-date)
                 (post-end end-date))

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
           ;; (meta (@ (http-equiv "Content-Type") (content "application/xhtml+xml")))
           (meta (@ (name viewport)
                    (content "width=device-width, initial-scale=0.5")))
           (meta (@ (name description)
                    (content "Calendar for the dates between " ,(date->string start-date)
                             " and " ,(date->string end-date))))
           ;; NOTE this is only for the time actually part of this calendar.
           ;; overflowing times from pre-start and post-end is currently ignored here.
           (meta (@ (name start-time)
                    (content ,(date->string start-date "~s"))))
           (meta (@ (name end-time)
                    (content ,(date->string  (date+ end-date (date day: 1)) "~s"))))
           ,(include-css "/static/tipped-4.7.0/dist/css/tipped.css")

           ,(include-css "/static/style.css")
           ,(include-alt-css "/static/dark.css"  '(title "Dark"))
           ,(include-alt-css "/static/light.css" '(title "Light"))

           (script (@ (src "/static/jquery-3.1.1.min.js")) "")
           (script (@ (src "/static/tipped-4.7.0/dist/js/tipped.min.js")) "")

           (script (@ (src "/static/script.js")) "")
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
                 (@ (style "grid-area: main"))
                 ,@(render-calendar calendars: calendars
                                    events: events
                                    start-date: start-date
                                    end-date: end-date
                                    pre-start: pre-start
                                    post-end: post-end
                                    next-start: next-start
                                    prev-start: prev-start
                                    ))

                ;; Page footer
                (footer
                 (@ (style "grid-area: footer"))
                 (span "Page generated " ,(date->string (current-date)))
                 (span (a (@ (href ,(repo-url) "/calparse"))
                          "Source Code"))
                 ,(let* ((long-hash short-hash (get-git-version))
                         (url (format #f "~a/calparse/commit/?id=~a"
                                      (repo-url) long-hash)))
                    `(span "Version " (a (@ (href ,url)) ,short-hash))))

                ;; Small calendar and navigation
                (nav (@ (class "calnav") (style "grid-area: nav"))
                     (div (@ (class "change-view"))
                          ,(btn href: (date->string
                                       (if (= 1 (day start-date))
                                           (start-of-week start-date (get-config 'week-start))
                                           start-date)
                                       "/week/~1.html")
                                "veckovy")

                          ,(btn href: (date->string (set (day start-date) 1) "/month/~1.html")
                                "månadsvy")

                          ,(btn id: "today-button"
                                href: (string-append
                                       "/today?" (case intervaltype
                                                    [(month) "view=month"]
                                                    [(week) "view=week"]))
                                "idag"))

                     (div (@ (class "jump-to"))
                          (form (@ (action "/today"))
                                (input (@ (type hidden)
                                          (name "view")
                                          (value ,(case intervaltype
                                                    [(month week) => symbol->string]
                                                    [else "month"]))))
                                (input (@ (type date)
                                          (name "date")
                                          (value (date->string start-date "~1"))))
                                ,(btn "➔"))))

                (details (@ (open) (style "grid-area: cal"))
                         (summary "Month overview")
                         (div (@ (class "smallcall-head"))
                              ,(string-titlecase (date->string start-date "~B ~Y")))
                         ;; NOTE it might be a good idea to put the navigation buttons
                         ;; earlier in the DOM-tree/tag order. At least Vimium's
                         ;; @key{[[} keybind sometimes finds parts of events instead.
                         (div (@ (class "smallcal"))
                              ;; prev button
                              ,(nav-link "«" (prev-start start-date))

                              ;; calendar table
                              (div ,(cal-table start-date: start-date end-date: end-date
                                               next-start: next-start
                                               prev-start: prev-start
                                               ))

                              ;; next button
                              ,(nav-link "»" (next-start start-date))))


                (div (@ (style "grid-area: details"))
                     ;; TODO only include these sliders in debug builds
                     (details (@ (class "sliders"))
                              (summary "Option sliders")
                              (label "Event blankspace")
                              ,(slider-input
                                variable: "editmode"
                                min: 0
                                max: 1
                                step: 0.01
                                value: 1)

                              (label "Fontsize")
                              ,(slider-input
                                unit: "pt"
                                min: 1
                                max: 20
                                step: 1
                                value: 8
                                variable: "event-font-size"))

                     ;; List of calendars
                     (details (@ (class "calendarlist"))
                              (summary "Calendar list")
                              (ul ,@(map
                                     (lambda (calendar)
                                       `(li (@ (class "CAL_bg_"
                                                 ,(html-attr (attr calendar 'NAME))))
                                            ,(attr calendar 'NAME)))
                                     calendars))))

                ;; List of events
                (div (@ (class "eventlist")
                        (style "grid-area: events"))
                     ;; Events which started before our start point,
                     ;; but "spill" into our time span.
                     (section (@ (class "text-day"))
                              (header (h2 "Tidigare"))
                              ,@(stream->list
                                 (stream-map
                                  fmt-single-event
                                  (stream-take-while
                                   (compose (cut date/-time<? <> start-date)
                                            (extract 'DTSTART))
                                   (cdr (stream-car evs))))))
                     ,@(stream->list (stream-map fmt-day evs))))))))


(define-method (html-chunked-main count start-date chunk-length)

  (define calendars (getf 'calendars))
  (define events (getf 'event-set))

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



(define-method (html-table-main count start-date)

  (define calendars (getf 'calendars))
  (define events (getf 'event-set))

  ;; TODO same file creation as in html-chunked-main
  (stream-for-each
   (lambda (start-of-month)
     (let ((fname (format #f "./html/~a.html" (date->string start-of-month "~1"))))
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

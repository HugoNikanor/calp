(define-module (html view calendar)
  :use-module (util)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (events-between))
  :use-module ((vcomponent build)
               :select (vcalendar vevent))
  :use-module (datetime)
  :use-module (html components)
  :use-module ((html vcomponent)
               :select (popup
                        calendar-styles
                        fmt-day
                        make-block
                        fmt-single-event
                                          ))
  :use-module (html config)
  :use-module (html util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module ((vcomponent group)
               :select (group-stream get-groups-between))
  :use-module ((git)
               :select (get-git-version))
  )

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
(define* (cal-table key: start-date end-date next-start prev-start)

  (define (td date)
    `(a (@ ,@(cond
              ;; We are before our time interval
              [(date< date start-date)
               ;; TODO find a prettier way to generate links to previous and next time intervals
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
                      (month-days (start-of-month start-date)))
         (events (append last-months current next)))
    `(div (@ (class "small-calendar"))
          (div (@ (class "column-head row-head")) "v.")
          ,@(map (lambda (d) `(div (@ (class "column-head"))
                              ,(string-titlecase (week-day-name d 2))))
                 (weekday-list))
          ,@(let ((first (start-of-week (car events)))
                  (last (start-of-week (last events))))
              (map (lambda (v) `(div (@ (class "row-head")) ,v))
                   (map (lambda (d) (week-number d))
                        (stream->list
                         (stream-take-while (lambda (s) (date<= s last))
                                            (week-stream first))))))
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

  ;; NOTE maybe don't do this again for every month
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

  ;; (display "<!doctype HTML>\n")
  (;;(@ (ice-9 pretty-print) pretty-print)
   (@ (sxml simple) sxml->xml)
   (xhtml-doc
    (@ (lang sv))
    (head
     (title "Calendar")
     (meta (@ (charset "utf-8")))
     ;; (meta (@ (http-equiv "Content-Type") (content "application/xhtml+xml")))
     (meta (@ (name viewport)
              (content "width=device-width, initial-scale=0.5")))
     (meta (@ (name description)
              (content "Calendar for the dates between "
                       ,(date->string start-date) " and "
                       ,(date->string end-date))))
     ;; NOTE this is only for the time actually part of this calendar.
     ;; overflowing times from pre-start and post-end is currently ignored here.
     (meta (@ (name start-time)
              (content ,(date->string start-date "~s"))))
     (meta (@ (name end-time)
              (content ,(date->string  (date+ end-date (date day: 1)) "~s"))))

     (script
      "EDIT_MODE=true;")

     (style ,(format #f "html {
    --editmode: 1.0;
    --event-font-size: 8pt;
    --gray: #757575;
    --btn-height: 0.5ex;
}"))

     ,(include-css "/static/style.css")
     ,(include-alt-css "/static/dark.css"  '(title "Dark"))
     ,(include-alt-css "/static/light.css" '(title "Light"))

     (script (@ (defer) (src "/static/script.js")))
     ,(calendar-styles calendars))

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
                              )

           ,@(for event in (stream->list
                            (events-between pre-start post-end events))
                  (popup event (string-append "popup" (html-id event)))))

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
                                     (start-of-week start-date)
                                     start-date)
                                 "/week/~1.html")
                          "veckovy")

                    ,(btn href: (date->string (set (day start-date) 1) "/month/~1.html")
                          "månadsvy")

                    ,(btn id: "today-button"
                          href: (string-append
                                 "/today?" (case intervaltype
                                             [(month) "view=month"]
                                             [(week) "view=week"]
                                             [else ""]))
                          "idag"))

               (div (@ (id "jump-to"))
                    ;; Firefox's accessability complain about each date
                    ;; component, meaning that it's broken. This label
                    ;; is for the whole input, which can be enabled
                    ;; if wanted.
                    ;; (label (@ (for "date")) "Hoppa till")
                    (form (@ (action "/today"))
                          (input (@ (type hidden)
                                    (name "view")
                                    (value ,(case intervaltype
                                              [(month week) => symbol->string]
                                              [else "month"]))))
                          (input (@ (type date)
                                    (name "date")
                                    (value ,(date->string start-date "~1"))))
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
               ,(when (or (debug) (edit-mode))
                  `(details (@ (class "sliders"))
                            (summary "Option sliders")


                            ,@(when (edit-mode)
                                `((label "Event blankspace")
                                  ,(slider-input
                                    variable: "editmode"
                                    min: 0
                                    max: 1
                                    step: 0.01
                                    value: 1)))

                            ,@(when (debug)
                                `((label "Fontsize")
                                  ,(slider-input
                                    unit: "pt"
                                    min: 1
                                    max: 20
                                    step: 1
                                    value: 8
                                    variable: "event-font-size")))))

               ;; List of calendars
               (details (@ (class "calendarlist"))
                        (summary "Calendar list")
                        (ul ,@(map
                               (lambda (calendar)
                                 `(li (@ (class "CAL_"
                                           ,(html-attr (prop calendar 'NAME))))
                                      ,(prop calendar 'NAME)))
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
               ,@(stream->list (stream-map fmt-day evs))))

     ;; This would idealy be a <template> element, but there is some
     ;; form of special case with those in xhtml, but I can't find
     ;; the documentation for it.
     ,@(let* ((cal (vcalendar
                    name: "Generated"
                    children: (list (vevent
                                     ;; The event template SHOULD lack
                                     ;; a UID, to stop potential problems
                                     ;; with conflicts when multiple it's
                                     ;; cloned mulitple times.
                                     dtstart: (datetime)
                                     dtend: (datetime)
                                     summary: ""
                                     ;; force a description field,
                                     ;; but don't put anything in
                                     ;; it.
                                     description: ""))))
              (event (car (children cal))))
         `((div (@ (class "template event-container") (id "event-template")
                   ;; Only needed to create a duration. So actual dates
                   ;; dosen't matter
                   (data-start "2020-01-01")
                   (data-end "2020-01-02"))
                ,(caddar          ; strip <a> tag
                  (make-block event `((class " generated ")))))
           ;; TODO merge this into the event-set, add attribute
           ;; for non-displaying elements.
           (div (@ (class "template") (id "popup-template"))
                ,(popup event (string-append "popup" (html-id event))))))))))

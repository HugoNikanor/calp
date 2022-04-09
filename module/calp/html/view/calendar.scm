(define-module (calp html view calendar)
  :use-module (hnh util)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (events-between))
  :use-module (datetime)
  :use-module (calp html components)
  :use-module ((calp html vcomponent)
               :select (calendar-styles
                        fmt-day
                        make-block
                        fmt-single-event
                        output-uid
                                          ))
  :use-module (calp html config)
  :use-module (calp html util)
  :use-module ((calp html caltable) :select (cal-table))

  :use-module (calp util config)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)

  :use-module ((vcomponent recurrence) :select (repeating? generate-recurrence-set))
  :use-module ((vcomponent util group)
               :select (group-stream get-groups-between))
  :use-module ((base64) :select (base64encode))

  :use-module (ice-9 format)
  :use-module (calp translation)
  )


;;; Main-stuff


;;; NOTE
;;; The side bar filters all earlier events for each day to not create repeats,
;;; and the html-generate procedure also filters, but instead to find earlier eventns.
;;; All this filtering is probably slow, and should be looked into.

;; TODO place this somewhere proper
(define repo-url (make-parameter "https://git.hornquist.se/calp"))


;; TODO document what @var{render-calendar} is supposed to take and return.
;; Can at least note that @var{render-calendar} is strongly encouraged to include
;; (script "const VIEW='??';"), where ?? is replaced by the name of the view.
(define*-public (html-generate
                 key:
                 (intervaltype 'all)    ; 'week | 'month | 'all
                 calendars  ; All calendars to work on, probably (get-calendars global-event-object)
                 events     ; All events which can be worked on, probably (get-event-set global-event-object)
                 start-date             ; First date in interval to show
                 end-date               ; Last  date in interval to show
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
    (scm-error 'misc-error "html-generate" (_ "Next-start needs to be a procedure") #f #f))

  (unless prev-start
    (scm-error 'misc-error "html-generate" (_ "Prev-start needs to be a procedure") #f #f))

  (xhtml-doc
   (@ (lang sv))
   (head
    (title "Calendar")
    (meta (@ (charset "utf-8")))
    ;; (meta (@ (http-equiv "Content-Type") (content "application/xhtml+xml")))
    (meta (@ (name viewport)
             (content "width=device-width, initial-scale=0.5")))
    (meta (@ (name description)
             (content ,(format #f (_ "Calendar for the dates between ~a and ~a")
                               ;; start date metainfo
                               (date->string start-date (_ "~Y-~m-~d"))
                               ;; end date metainfo
                               (date->string end-date   (_ "~Y-~m-~d"))))))
    ;; NOTE this is only for the time actually part of this calendar.
    ;; overflowing times from pre-start and post-end is currently ignored here.
    (meta (@ (name start-time)
             (content ,(date->string start-date "~s"))))
    (meta (@ (name end-time)
             (content ,(date->string  (date+ end-date (date day: 1)) "~s"))))

    (script
     ,(format #f
              "
EDIT_MODE=~:[false~;true~];
window.default_calendar='~a';"
              (edit-mode)
              (base64encode ((@ (vcomponent) default-calendar)))))


    (style ,(format #f "html {
    --editmode: 1.0;
    --event-font-size: 8pt;
}"))

    ,(include-css "/static/style.css")
    ,(include-alt-css "/static/dark.css"  '(title "Dark"))
    ,(include-alt-css "/static/light.css" '(title "Light"))

    (script (@ (src "/static/script.out.js")))
    (script (@ (src "/static/user/user-additions.js")))

    ,(calendar-styles calendars)

    ,@(when (debug)
        '((style ":root { --background-color: pink; }"))))

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

          ,(btn onclick: "addNewEvent()"
                "+")

          ;; Popups used to be here, but was moved into render-calendar so each
          ;; sub-view can itself decide where to put them. This is important
          ;; since they need to be placed as children to the scrolling
          ;; component, if one such component exists.
          )

         ;; Page footer
         (footer
          (@ (style "grid-area: footer"))
          (span ,(_ "Page generated ")
                ;; Generation data
                ,(date->string (current-date) (_ "~Y-~m-~d")))
          (span ,(_ "Current time ") (current-time (@ (interval 1))))
          (span (a (@ (href ,(repo-url)))
                   ,(_ "Source Code"))))

         ;; Small calendar and navigation
         (nav (@ (class "calnav") (style "grid-area: nav"))
              (div (@ (class "change-view"))
                   ,(btn href: (date->string
                                (if (= 1 (day start-date))
                                    (start-of-week start-date)
                                    start-date)
                                "/week/~1.html")
                         ;; Button to view week
                         (_ "Week"))

                   ,(btn href: (date->string (set (day start-date) 1) "/month/~1.html")
                         ;; button to view month
                         (_ "Month"))

                   (today-button
                    (a (@ (class "btn")
                          (href ,(string-append
                                  "/today?" (case intervaltype
                                              [(month) "view=month"]
                                              [(week) "view=week"]
                                              [else ""]))))
                       ;; Button to go to today
                       ,(_ "Today"))))

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
                  (summary ,(_ "Month overview"))
                  (div (@ (class "smallcall-head"))
                       ,(string-titlecase (date->string start-date
                                                        ;; Header of small calendar
                                                        (_ "~B ~Y"))))
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

              ;; TODO Style this from as all other input forms in the sidebar.
              (form (@ (class "simplesearch")
                       (action "/search/text"))
                    (input (@ (type "text")
                              (name "q")
                              ;; Search placeholder
                              (placeholder ,(_ "Search"))))
                    (input (@ (type "submit")
                              (value ">"))))

              ,(when (or (debug) (edit-mode))
                 `(details (@ (class "sliders"))
                           (summary ,(_ "Option sliders"))

                           ,@(when (edit-mode)
                               `((label ,(_ "Event blankspace"))
                                 ,(slider-input
                                   variable: "editmode"
                                   min: 0
                                   max: 1
                                   step: 0.01
                                   value: 1)))

                           ,@(when (debug)
                               `((label ,(_ "Fontsize"))
                                 ,(slider-input
                                   unit: "pt"
                                   min: 1
                                   max: 20
                                   step: 1
                                   value: 8
                                   variable: "event-font-size")))))

              ;; List of calendars
              (details (@ (class "calendarlist"))
                       (summary ,(_ "Calendar list"))
                       (ul ,@(map
                              (lambda (calendar)
                                `(li (@ (data-calendar ,(base64encode (prop calendar 'NAME))))
                                     (a (@ (href "/search?"
                                                 ,((@ (web uri-query) encode-query-parameters)
                                                   `((q . (and (date/-time<=?
                                                                ,(current-datetime)
                                                                (prop event 'DTSTART))
                                                               ;; TODO this seems to miss some calendars,
                                                               ;; I belive it's due to some setting X-WR-CALNAME,
                                                               ;; which is only transfered /sometimes/ into NAME.
                                                               (string=? ,(->string (prop calendar 'NAME))
                                                                         (or (prop (parent event) 'NAME) ""))))))))
                                        ,(prop calendar 'NAME))))
                              calendars))
                       ;; (div (@ (id "calendar-dropdown-template") (class "template"))
                       ;;      )
                       ))

         ;; List of events
         (div (@ (class "eventlist")
                 (style "grid-area: events"))
              ;; Events which started before our start point,
              ;; but "spill" into our time span.
              (section (@ (class "text-day"))
                       (header (h2 ,(_ "Earlier")))
                       ;; TODO this group gets styles applied incorrectly.
                       ;; Figure out way to merge it with the below call.
                       ,@(stream->list
                          (stream-map
                           (lambda (ev)
                             (fmt-single-event
                              ev `((id ,(html-id ev))
                                   (data-calendar ,(base64encode (or (prop (parent ev) 'NAME) "unknown"))))))
                           (stream-take-while
                            (compose (cut date/-time<? <> start-date)
                                     (extract 'DTSTART))
                            (cdr (stream-car evs))))))
              ,@(stream->list (stream-map fmt-day evs))))

    ;; This would idealy be a <template> element, but there is some
    ;; form of special case with those in xhtml, but I can't find
    ;; the documentation for it.
    ;; ,@(let* ((cal (vcalendar
    ;;                name: "Generated"
    ;;                children: (list (vevent
    ;;                                 ;; The event template SHOULD lack
    ;;                                 ;; a UID, to stop potential problems
    ;;                                 ;; with conflicts when multiple it's
    ;;                                 ;; cloned mulitple times.
    ;;                                 dtstart: (datetime)
    ;;                                 dtend: (datetime)
    ;;                                 summary: ""
    ;;                                 ;; force a description field,
    ;;                                 ;; but don't put anything in
    ;;                                 ;; it.
    ;;                                 description: ""))))
    ;;          (event (car (children cal))))
    ;;     `(
    ;;       ;; (div (@ (class "template event-container") (id "event-template")
    ;;       ;;         ;; Only needed to create a duration. So actual dates
    ;;       ;;         ;; dosen't matter
    ;;       ;;         (data-start "2020-01-01")
    ;;       ;;         (data-end "2020-01-02"))
    ;;       ;;      ,(caddar          ; strip <a> tag
    ;;       ;;        (make-block event `((class " generated ")))))
    ;;       ;; TODO merge this into the event-set, add attribute
    ;;       ;; for non-displaying elements.
    ;;       ;; (div (@ (class "template") (id "popup-template"))
    ;;       ;;      ,(popup event (string-append "popup" (html-id event))))
    ;;       ))

    ;;; Templates used by our custom components
    ,((@ (calp html vcomponent) edit-template) calendars)
    ,((@ (calp html vcomponent) description-template))
    ,((@ (calp html vcomponent) vevent-edit-rrule-template))
    ,((@ (calp html vcomponent) popup-template))

    ;; Auto-complets when adding new fields to a component
    ;; Any string is however still valid.
    (datalist (@ (id "known-fields"))
              ,@(map (lambda (f)
                       `(option (@ (value ,f))))
                     '(CALSCALE
                       METHOD PRODID VERSION ATTACH
                       CATEGORIES CLASS COMMENT
                       DESCRIPTION GEO LOCATION
                       PERCENT-COMPLETE PRIORITY
                       RESOURCES STATUS SUMMARY
                       COMPLETED DTEND DUE DTSTART
                       DURATION FREEBUSY
                       TRANSP TZID TZNAME
                       TZOFFSETFROM TZOFFSETTO
                       TZURL ATTENDEE CONTACT
                       ORGANIZER RECURRENCE-ID
                       RELATED-TO URL EXDATE
                       RDATE RRULE ACTION REPEAT
                       TRIGGER CREATED DTSTAMP LAST-MODIFIED
                       SEQUENCE REQUEST-STATUS
                       )))

    ,@(let* (
             (flat-events
              ;; A simple filter-sorted-stream on event-overlaps? here fails.
              ;; See tests/annoying-events.scm
              (stream->list
               (stream-filter
                (lambda (ev)
                  ((@ (vcomponent datetime) event-overlaps?)
                   ev pre-start
                   (date+ post-end (date day: 1))))
                (stream-take-while (lambda (ev) (date<
                                            (as-date (prop ev 'DTSTART))
                                            (date+ post-end (date day: 1))))
                                   events))))
             (repeating% regular (partition repeating? flat-events))
             (repeating
              (for ev in repeating%
                   (define instance (copy-vcomponent ev))

                   (set! (prop instance 'UID) (output-uid instance))
                   (delete-parameter! (prop* instance 'DTSTART) '-X-HNH-ORIGINAL)
                   (delete-parameter! (prop* instance 'DTEND)   '-X-HNH-ORIGINAL)

                   instance)))

        `(
          ;; Mapping showing which events belongs to which calendar,
          ;; on the form
          ;; (calendar (@ (key ,(base64-encode calendar-name)))
          ;;           (li ,event-uid) ...)
          (div (@ (style "display:none !important;")
                  (id "calendar-event-mapping"))
               ,(let ((ht (make-hash-table)))
                  (for-each (lambda (event)
                              (define name (prop (parent event) 'NAME))
                              (hash-set! ht name
                                         (cons (prop event 'UID)
                                               (hash-ref ht name '()))))
                            (append regular repeating))

                  (hash-map->list
                   (lambda (key values)
                     `(calendar (@ (key ,(base64encode key)))
                                ,@(map (lambda (uid) `(li ,uid))
                                       values)))
                   ht)))

          ;; Calendar data for all events in current interval,
          ;; rendered as xcal.
          (div (@ (style "display:none !important;")
                   (id "xcal-data"))
                ,((@ (vcomponent formats xcal output) ns-wrap)
                  (map (@ (vcomponent formats xcal output) vcomponent->sxcal)
                       (append regular repeating)))))))))

(define-module (calp html view calendar)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util type)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (instance-start-datetime
                        instance-overlaps?))
  :use-module (datetime)
  :use-module ((calp html util) :select (html-file-extension))
  :use-module (calp html components)
  :use-module ((calp html vcomponent)
               :select (calendar-styles fmt-day))
  :use-module ((calp html caltable) :select (cal-table))

  :use-module (calp util config)

  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-41) :select (stream->list))
  :use-module (srfi srfi-71)

  :use-module ((base64) :select (base64encode))

  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module (calp translation)
  :use-module (vcomponent data-stores common)
  :use-module ((vcomponent data-stores query)
               :select (entries-between))

  :export (html-generate)
  )


;;; Main-stuff


;;; NOTE
;;; The side bar filters all earlier events for each day to not create repeats,
;;; and the html-generate procedure also filters, but instead to find earlier eventns.
;;; All this filtering is probably slow, and should be looked into.

;; TODO place this somewhere proper
(define repo-url (make-parameter "https://git.hornquist.se/calp"))


;; Generates a complete HTML calendar page as an SXML document
;; (including *TOP* and *PI* nodes).
;;
;; Arguments:
;; - intervaltype:
;;   Type of interval this page represents
;;   one of week, month, or all
;;   Only used to construct some queries.
;;   TODO remove this parameter, and get it from context, or at least
;;        pair it with the render-calendar procedure
;; - calendars:
;;   list of named data stores, which will be queried to get the event set
;; - start-date, end-date, next-start, prev-start
;;   Interval we which should be rendered.
;;   This is given as 4 dates, since it sometimes makes sencse to include "extra"
;;   entries before or after the interval.
;;   Consider Jaruary 2026, with weeks starting on mondays:
;;            januari 2026
;;        må ti on to fr lö sö
;;                  1  2  3  4
;;         5  6  7  8  9 10 11
;;        12 13 14 15 16 17 18
;;        19 20 21 22 23 24 25
;;        26 27 28 29 30 31
;;   start and end date would obviously be 1 and 31 january, while
;;   pre-start would be 2025-12-29 and post-end 2026-02-01, since
;;   these are still there.
;;   Event list will be generated from [pre-start, post-end],
;;   and all four will be passed to `render-calendar` for it to use at it sees fit.
;;   Must satisfy pre-start <= start-date < end-date <= post-end
;; - next-start, prev-start
;;   procedures which maps dates to dates. Used to get the next or
;;   previous page start, and will be called with the current start date
;;   value.
;; - render-calendar:
;;   Callback to render the graphical part of the calendar.
;;   Will be called with the following keyword arguments:
;;   - stores, forwarded from `calendars`
;;   - start-date
;;   - end-date (inclusive)
;;   - next-start
;;   - prev-start
;;   - target-timezone
;; Implementations are free to use any of these fields they see fit
;;   return: list of html elemnts, coded as sexp objects.
;; TODO end-date, next-start, and prev-start should all follow from intervaltype
;; TODO instead of having intervaltype separate, create a compound type, changing the
;; signature of html-generate to
;; - calendars: (list-of store)
;; - start-date: date? (interpreted by the renderer)
;; - target-timezone: string?
;; - renderer: (tuple (end-date (procedure () (date)))
;;                    ...)
(define* (html-generate
          key:
          (intervaltype 'all)
          calendars ; All data-stores to work on (name is historical and subject to change)
          start-date                ; First date in interval to show
          end-date                  ; Last  date in interval to show
          render-calendar           ; (bunch of kv args) → (list sxml)
          next-start                ; date → date
          prev-start                ; date → date
          (target-timezone "UTC"))
  (typecheck intervaltype (memv '(week month all)))
  (typecheck calendars (list-of (pair-of string? calendar-data-store?)))
  (typecheck start-date date?)
  (typecheck end-date date?)
  (typecheck render-calendar procedure?)
  (typecheck next-start procedure?)
  (typecheck prev-start procedure?)
  (typecheck target-timezone string?)

  (define (nav-link display date)
    `(a (@ (href ,(date->string date "~Y-~m-~d.") ,(html-file-extension))
           (class "nav hidelink"))
        (div (@ (class "nav"))
             ,display)))

  (xhtml-doc
   (@ (lang sv))
   (head
    (title "Calendar")
    (meta (@ (charset "utf-8")))
    ;; (meta (@ (http-equiv "Content-Type") (content "application/xhtml+xml")))
    (meta (@ (name viewport)
             (content "width=device-width, initial-scale=0.5")))
    (meta (@ (name description)
             (content ,(format #f (G_ "Calendar for the dates between ~a and ~a")
                               (date->string start-date (G_ "~Y-~m-~d"))
                               (date->string end-date   (G_ "~Y-~m-~d"))))))
    (meta (@ (name start-time)
             (content ,(date->string start-date "~s"))))
    (meta (@ (name end-time)
             (content ,(date->string  (date+ end-date (date day: 1)) "~s"))))

    (script
     ;; TODO this is just ugly
     ,(lambda () (format #t "
EDIT_MODE=~:[false~;true~];
window.default_calendar='~a';"
                    ((@ (calp html config) edit-mode))
                    (base64encode ((@ (vcomponent config) default-calendar))))))


    ;; TODO call --editmode something more descriptive,
    ;; it's used to add some blank space to the right of components when dragging
    (style ,(format #f "html {
    --editmode: 1.0;
    --event-font-size: 8pt;
}"))

    ;; TODO the prefix for static resources needs to be configurable,
    ;; Currently production environments expect "/static/", while the
    ;; development environment requires "/static/out/".

    ,(include-css "/static/out/style.css")
    ,(include-alt-css "/static/out/dark.css"  '(title "Dark"))
    ,(include-alt-css "/static/out/light.css" '(title "Light"))

    (script (@ (src "/static/out/script.js")))
    (script (@ (src "/static/out/user/user-additions.js")))

    (style ,(lambda () (calendar-styles calendars #t)))

    ,@(when ((@ (calp html config) debug))
        '((style ":root { --background-color: pink; }"))))

   (body
    (div (@ (class "root"))
         (main
          ;; Actuall calendar
          (@ (style "grid-area: main"))
          ,@(render-calendar stores: calendars
                             start-date: start-date
                             end-date: end-date
                             next-start: next-start
                             prev-start: prev-start
                             target-timezone: target-timezone
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
          (span ,(G_ "Page generated ")
                ,(datetime->string (current-datetime) (G_ "~Y-~m-~d ~H:~M:~S~Z")))
          (span ,(G_ "Current time ") (current-time (@ (interval 1))))
          (span ,(format #f (G_ "TZ: ~a") target-timezone))
          (span (a (@ (href ,(repo-url)))
                   ,(G_ "Source Code"))))

         ;; Small calendar and navigation
         (nav (@ (class "calnav") (style "grid-area: nav"))
              (div (@ (class "change-view"))
                   ,(btn href: (string-append
                                (date->string
                                 ;; TODO this seems wrongly designed
                                 (if (= 1 (day start-date))
                                     (start-of-week start-date)
                                     start-date)
                                 "/week/~1.")
                                (html-file-extension))
                         ;; Button to view week
                         (G_ "Week"))

                   ,(btn href: (string-append (date->string (day start-date 1) "/month/~1.")
                                              (html-file-extension))
                         ;; button to view month
                         (G_ "Month"))

                   (today-button
                    (a (@ (class "btn")
                          (href ,(string-append
                                  "/today?" (case intervaltype
                                              [(month) "view=month"]
                                              [(week) "view=week"]
                                              [else ""]))))
                       ;; Button to go to today
                       ,(G_ "Today"))))

              (date-jump
               ;; Firefox's accessability complain about each date
               ;; component, meaning that it's broken. This label
               ;; is for the whole input, which can be enabled
               ;; if wanted.
               ;; (label (@ (for "date")) "Hoppa till")
               (form (@ (action "/today"))
                     (input (@ (type "hidden")
                               (name "view")
                               (value ,(case intervaltype
                                         [(month week) => symbol->string]
                                         [else "month"]))))
                     (input (@ (type "date")
                               (name "date")
                               (value ,(date->string start-date "~1"))))
                     ,(btn "➔"))))

         (details (@ (open) (style "grid-area: cal"))
                  (summary ,(G_ "Month overview"))
                  (div (@ (class "smallcall-head"))
                       ,(string-titlecase (date->string start-date
                                                        ;; Header of small calendar
                                                        (G_ "~B ~Y"))))
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
                              (placeholder ,(G_ "Search"))))
                    (input (@ (type "submit")
                              (value ">"))))

              ,(when (or ((@ (calp html config) debug))
                         ((@ (calp html config) edit-mode)))
                 `(details (@ (class "sliders"))
                           (summary ,(G_ "Option sliders"))

                           (form
                            (label (@ (for "default-tz")) ,(G_ "Default Timezone"))
                            (select (@ (id "default-tz")
                                       (name "tz"))
                              ;; TODO generate this list from the loaded zoneinfo database
                              (option "UTC")
                              (option "Europe/Stockholm")
                              (option "America/New_York"))

                            (input (@ (type "submit"))))

                           ,@(when ((@ (calp html config) edit-mode))
                               `((label ,(G_ "Event blankspace"))
                                 ,(slider-input
                                   variable: "editmode"
                                   min: 0
                                   max: 1
                                   step: 0.01
                                   value: 1)))

                           ,@(when ((@ (calp html config) debug))
                               `((label ,(G_ "Fontsize"))
                                 ,(slider-input
                                   unit: "pt"
                                   min: 1
                                   max: 20
                                   step: 1
                                   value: 8
                                   variable: "event-font-size")))))

              ;; List of calendars
              (details (@ (class "calendarlist"))
                       (summary ,(G_ "Calendar list"))
                       (ul ,@(map
                              (lambda (calendar)
                                `(li (@ (data-calendar ,(base64encode (car calendar))))
                                     (a (@ (href "/search?"
                                                 ,((@ (web query) encode-query-parameters)
                                                   `((q . (and (datetime<=?
                                                                ,(current-datetime)
                                                                (instance-start-datetime event))
                                                               ;; TODO
                                                               ;; this is broken, since we can't access the parent of an event
                                                               #;
                                                               (string=? ,(car calendar)
                                                                         (or (prop (parent event) 'NAME) ""))))))))
                                        ,(or (store-displayname (cdr calendar))
                                             (car calendar)))))
                              calendars))))

         ;; List of event in sidebar.
         ;; Used for no-script intrecation, and as a Ctrl-F friendly
         ;; search (since it includes description and the like)
         ;; TODO this must be of sufficient height for the UI to not break.
         ;;      This means that having an empty calendar triggers a bug.

         (div (@ (class "eventlist")
                 (style "grid-area: events"))

              ,@(let ()
                  (define events
                    (map (lambda (ev) (modify ev (ref 2) (compose car vcomponent-children)))
                         (stream->list (apply entries-between
                                              target-timezone
                                              ;; NOTE previously, the overflows where configurable.
                                              ;; However, only very specific values makes sence for
                                              ;; specific configurations.
                                              ;; start/end-of-week is choosen, since it does
                                              ;; what we want for month views, and is effectively
                                              ;; a no-op for week views
                                              (datetime date: (start-of-week start-date)
                                                        tz: target-timezone)
                                              (datetime date: (date+ (end-of-week end-date) (date day: 1))
                                                        tz: target-timezone)
                                              calendars))))
                  (cons
                   ;; Events which started before our start point,
                   ;; but "spill" into our time span.
                   (fmt-day
                    target-timezone
                    (G_ "Earlier")
                    (filter (match-lambda
                              ((_ _ ev) (datetime</zoneinfo
                                         (instance-start-datetime target-timezone ev)
                                         (datetime date: (start-of-week start-date)
                                                   tz: target-timezone))))
                            events))
                   (map (lambda (start)
                          (fmt-day
                           target-timezone
                           (date->string start (G_ "~Y-~m-~d"))
                           (filter (match-lambda
                                     ((_ _ ev)
                                      (and (instance-overlaps?
                                            target-timezone
                                            ev
                                            (datetime date: start tz: target-timezone)
                                            (datetime date: (date+ start (date day: 1))
                                                      tz: target-timezone))
                                           ;; If start was an earlier day
                                           ;; This removes all descriptions from
                                           ;; events for previous days,
                                           ;; solving duplicates.
                                           ;; TODO this also filters out long events starting that day.
                                           (datetime</zoneinfo
                                            (datetime date: start tz: target-timezone)
                                            (instance-start-datetime target-timezone ev)))))
                                   events)))
                        (date-range (start-of-week start-date)
                                    (end-of-week end-date)))))))

    ;; Templates used by our custom components
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
    )))



;;; Old Stuff for embedding components as SXML directly into page payload
;; ,@(let* (
;;          ;; TODO events-in-interval from store
;;          (flat-events
;;           ;; A simple filter-sorted-stream on instance-overlaps? here fails.
;;           ;; See tests/annoying-events.scm
;;           (stream->list
;;            (stream-filter
;;             (lambda (ev)
;;               ((@ (vcomponent datetime) instance-overlaps?)
;;                ev pre-start
;;                (date+ post-end (date day: 1))))
;;             (stream-take-while (lambda (ev) (date<
;;                                         (as-date (prop1 ev 'DTSTART))
;;                                         (date+ post-end (date day: 1))))
;;                                events))))

;;          (repeating% regular (partition recurring? flat-events))

;;          (repeating
;;           (for ev in repeating%
;;                ;; TODO *why* are we removing -X-HNH-ORIGINAL here?
;;                (-> ev
;;                    ;; TODO vline wrapper?
;;                    (set (prop* 'UID) (just (output-uid ev)))
;;                    (modify (lens-compose (prop* 'DTSTART) vline-parameters*)
;;                            (lambda (params) (table-remove params '-X-HNH-ORIGINAL)))
;;                    (modify (lens-compose (prop* 'DTEND) vline-parameters*)
;;                            (lambda (params) (table-remove params '-X-HNH-ORIGINAL)))))))


;;     `(
;;       ;; Mapping showing which events belongs to which calendar,
;;       ;; on the form
;;       ;; (calendar (@ (key ,(base64-encode calendar-name)))
;;       ;;           (li ,event-uid) ...)
;;       (div (@ (style "display:none !important;")
;;               (id "calendar-event-mapping"))
;;            ,(let ((ht (make-hash-table)))
;;               (for-each (lambda (event)
;;                           (define name (prop (parent event) 'NAME))
;;                           (hash-set! ht name
;;                                      (cons (prop event 'UID)
;;                                            (hash-ref ht name '()))))
;;                         (append regular repeating))

;;               (hash-map->list
;;                (lambda (key values)
;;                  `(calendar (@ (key ,(base64encode key)))
;;                             ,@(map (lambda (uid) `(li ,uid))
;;                                    values)))
;;                ht)))

;;       ;; Calendar data for all events in current interval,
;;       ;; rendered as xcal.
;;       (div (@ (style "display:none !important;")
;;               (id "xcal-data"))
;;            ,(lambda ()
;;               (let ((serializer ((@ (vcomponent media-type) serializer)
;;                                  (@ (vcomponent media-type application calendar+xml) format))))
;;                 (serializer
;;                  ((@ (vcomponent create) vcalendar)
;;                   prodid: "TODO prodid"
;;                   version: "2.0"
;;                   (append regular repeating))
;;                  (current-output-port)
;;                  envelope?: #f)))))))))

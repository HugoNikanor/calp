(define-module (calp html vcomponent)
  :use-module (hnh util)
  ;; TODO should we really use path-append here? Path append is
  ;; system-dependant, while URL-paths aren't.
  :use-module ((hnh util path) :select (path-append))
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (hnh util type)
  :use-module (hnh util color)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module ((rnrs io ports) :select (put-bytevector))
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module ((text util) :select (add-enumeration-punctuation))
  :use-module ((web query) :select (encode-query-parameters))
  :use-module ((web uri) :select (uri? uri->string))
  :use-module ((calp html util) :select (html-id calculate-fg-color))
  :use-module ((calp html config) :select (edit-mode debug))
  :use-module ((crypto) :select (sha256 checksum->string))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module ((vcomponent type recurrence) :select (recurring?))
  :use-module ((vcomponent datetime output)
               :select (
                        format-recurrence-rule
                                      ))
  :use-module (vcomponent data-stores common)
  :use-module (calp util config)
  :use-module ((base64) :select (base64encode))
  :use-module (ice-9 format)
  :use-module (calp translation)
  :use-module ((scheme base) :select (bytevector?))
  :export (format-summary
           format-description
           compact-event-list
           fmt-single-event
           fmt-day
           calendar-styles
           make-block
           output-uid
           edit-template
           description-template
           vevent-edit-rrule-template
           popup-template
           ))


;; Encodes string as a series of xml entities.
;; Recommended when sending complex unicode characters over to bad
;; clients.
(define (xml-entities s)
  (lambda ()
    (for-each display
              (map (lambda (c) (format #f "&#x~x;" (char->integer c)))
                   (string->list s)))))

;; Format the event summary
(define (format-summary ev str)
  (typecheck ev vevent?)
  (typecheck str string?)
  (((@ (calp html filter) summary-filter)) ev str))

;; NOTE this should have information about context (html/term/...)
;; And then be moved somewhere else.
(define (format-description ev str)
  (typecheck ev vevent?)
  (typecheck str string?)
  (catch* (lambda () (((@ (calp html filter) description-filter)) ev str))
          (configuration-error
           (lambda (key subr msg args data)
             (format (current-error-port)
                     (G_ "Error retrieving configuration, ~?~%") msg args)))
          (#t ; for errors when running the filter
           (lambda (err . args)
             (warning (G_ "~a on formatting description, ~s") err args)
             str))))

;; Takes a vline containing an inline image.
;; NOTE that a check that FMTTYPE is set to "image/*" MUST be done
;; beforehand by the caller.
(define (attach-inline-image attach)
  (typecheck attach vline?)
  `(img (@ (class "attach")
           ;; Should be set in the CSS, but better safe than sorry in
           ;; case of large images.
           (style "max-width: 100%")
           (src ,(format #f "data:~a;base64,~a"
                         (param attach 'FMTTYPE)
                         (base64encode (vline-value attach)))))))

;; used by search view
(define (compact-event-list list)
  (typecheck list (list-of vevent?))

  ;; (define calendars
  ;;  (delete-duplicates!
  ;;   (filter (lambda (x) (eq? 'VCALENDAR (type x)))
  ;;           (map parent list))
  ;;   eq?))

  ;; TODO
  (define calendars '())

  (define (summary event)
    `(summary (div (@ (class "summary-line "))
                   (span (@ (class "square")
                            (data-calendar
                             ,(base64encode
                               (or #; (prop (parent event) 'NAME)
                                   "unknown")))))
                   (time ,(let ((dt (prop1 event 'DTSTART)))
                            (if (datetime? dt)
                                (datetime->string dt (G_ "~Y-~m-~d ~H:~M"))
                                (date->string dt (G_ "~Y-~m-~d") ))))
                   (a (@ (href ,(date->string (as-date (prop1 event 'DTSTART)) "/week/~Y-~m-~d.html")))
                      ;; Button for viewing calendar, accompanied by a calendar icon
                      ;; TODO fragment focusing that specific event
                      ,(G_ "View") " 📅")
                   (span ,(prop1 event 'SUMMARY)))))
  (cons
   `(style ,(lambda () (calendar-styles calendars #t)))
   (for event in list
        `(details
          ,(summary event)
          ;; TODO better format
          ,(fmt-single-event event)))))




;; TODO localize this?
(define (format-event-time-span ev)
  (typecheck ev vevent?)

  ;; Takes an event, and returns a pretty string for the time interval
  ;; the event occupies.
  (define (fmt-time-span ev)
    (typecheck ev vevent?)
    (cond [(prop1 ev 'DTSTART) date?
           => (lambda (s)
                ;; TODO duration
                (cond [(prop1 ev 'DTEND)
                       => (lambda (e)
                            ;; start = end, only return one value
                            (if (date= e (date+ s (date day: 1)))
                                (G_ "~Y-~m-~d")
                                (values (G_ "~Y-~m-~d")
                                        (G_ "~Y-~m-~d"))))]
                      ;; no end value, just return start
                      [else (date->string s)]))]
          [else ; guaranteed datetime
           (let ((s (prop1 ev 'DTSTART))
                 (e (prop1 ev 'DTEND)))
             ;; TODO duration
             (if e
                 (let ((fmt-str (if (date= (datetime-date s) (datetime-date e))
                                    (G_ "~H:~M")
                                    ;; Note the non-breaking space
                                    (G_ "~Y-~m-~d ~H:~M"))))

                   (values fmt-str fmt-str))
                 ;; Note the non-breaking space
                 (G_ "~Y-~m-~d ~H:~M")))]))

  (call-with-values (lambda () (fmt-time-span ev))
    (case-lambda [(start)
                  `(div (time (@ (class "dtstart")
                                 (data-property "dtstart")
                                 (data-fmt ,(string-append "~L" start))
                                 (datetime ,(datetime->string
                                             (as-datetime (prop1 ev 'DTSTART))
                                             "~1T~3")))
                              ,(datetime->string
                                (as-datetime (prop1 ev 'DTSTART))
                                start)))]
                 [(start end)
                  `(div (time (@ (class "dtstart")
                                 (data-property "dtstart")
                                 (data-fmt ,(string-append "~L" start))
                                 (datetime ,(datetime->string
                                             (as-datetime (prop1 ev 'DTSTART))
                                             "~1T~3")))
                              ,(datetime->string (as-datetime (prop1 ev 'DTSTART))
                                                 start))
                        " — "
                        (time (@ (class "dtend")
                                 (data-property "dtend")
                                 (data-fmt ,(string-append "~L" end))
                                 (datetime ,(datetime->string
                                             (as-datetime (prop1 ev 'DTSTART))
                                             "~1T~3")))
                              ,(datetime->string (as-datetime (prop1 ev 'DTEND))
                                                 end)))])))

;; Format event as text.
;; Used in
;; - sidebar
;; - popup overwiew tab
;; - search result (event details)
;; Note that the <vevent-description/> tag is bound as a JS custem element, which
;; will re-render all this, through description-template. This also means that
;; the procedures output is intended to be static, and to NOT be changed by JavaScript.
(define* (fmt-single-event ev
                           optional: (attributes '())
                           key: (fmt-header list))
  (typecheck ev vevent?)
  ;; Sholud be (list-of (pair-of symbol? any-type?))
  ;; but sxml accepts almost anything
  (typecheck attributes (list-of (pair-of symbol? any-type)))
  (typecheck fmt-header procedure?)

  `(vevent-description
    (@ ,@(assq-merge
          attributes
          `(
            (class ,(when (and (prop1 ev 'PARTSTAT)
                               (eq? 'TENTATIVE (prop1 ev 'PARTSTAT)))
                      " tentative "))
            (data-uid ,(output-uid ev)))))
    (div (@ (class "vevent eventtext summary-tab"))
         (h3 ,(fmt-header
               (when (prop% ev 'RRULE)
                 `(span (@ (class "repeating")) "↺"))
               `(span (@ (class "summary")
                         (data-property "summary"))
                      ,(prop1 ev 'SUMMARY))))
         (div
          ,(format-event-time-span ev)

          (div (@ (class "fields"))
               ,(awhen (prop% ev 'LOCATION)
                  `(div (b ,(G_ "Location: "))
                        ;; TODO support for multiple locations?
                        ;; Doesn't seem to be allowed by the standard, but it should work
                        ;; anyways.
                        (div (@ (class "location") (data-property "location"))
                             ;; TODO altrep
                             ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                          (vline-value (car it))))))

               ,(awhen (prop% ev 'DESCRIPTION)
                       `(div (@ (class "description")
                                (data-property "description"))
                             ;; TODO altrep
                             ;; TODO language
                             ,(format-description ev (vline-value (car it)))))

               ,@(awhen (prop% ev 'ATTACH)
                        ;; attach satisfies @code{vline?}
                        (for attach in it
                             (define v (vline-value attach))
                             (cond
                              ((bytevector? v)
                               ;; TODO guess datatype if FMTTYPE is missing
                               (let ((fmt-type (and=> (param attach 'FMTTYPE)
                                                      (lambda (p) (string-split p #\/)))))
                                 ;; TODO other file formats
                                 (cond ((and fmt-type
                                             (not (null? fmt-type))
                                             (string=? "image" (car fmt-type)))
                                        (attach-inline-image attach))
                                       (else `(pre "As of yet unsupported file format" ,fmt-type)))))
                              ((uri? v)
                               (let ((fmt-type (and=> (param attach 'FMTTYPE)
                                                      (lambda (p) (string-split p #\/)))))
                                 (cond ((and fmt-type
                                             (not (null? fmt-type))
                                             (string=? "image" (car fmt-type)))
                                        `(img (@ (class "attach")
                                                 (src ,(uri->string v)))))
                                       (else `(a (@ (class "attach")
                                                    (href ,(uri->string v)))
                                                 ,(uri->string v))))))

                               ;; Neither BINARY nor URI
                               (else (scm-error 'misc-error "fmt-single-event"
                                                "Unknown attachement type ~s, expected BINARY or URI"
                                                (list (and=> (param attach 'VALUE) car))
                                                #f)))))

               ,(awhen (prop% ev 'CATEGORIES)
                       ;; TODO language
                       `(div (@ (class "categories"))
                             ,@(map (lambda (c)
                                      `(a (@ (class "category")
                                             ;; TODO centralize search terms
                                             (href
                                              "/search/?"
                                              ,(encode-query-parameters
                                                `((q . ,(format #f "~s"
                                                                `(member
                                                                  ,(->string c)
                                                                  (or (map vline-value (prop% event 'CATEGORIES))
                                                                      '()))))))))
                                          ,(vline-value c)))
                                    it)))

               ,(when (prop1 ev 'RRULE)
                  `(div (@ (class "rrule"))
                        ,@(format-recurrence-rule ev)))

               ,(awhen (prop1 ev 'LAST-MODIFIED)
                  `(div (@ (class "last-modified")) ,(G_ "Last modified") " "
                        ,(datetime->string it
                                           ;; Last modified datetime
                                           (G_ "~1 ~H:~M")))))

          ))))



;; Single event in side bar (text objects)
(define (fmt-day header entries)
  (typecheck header string?)
  (typecheck entries (list-of (tuple-of string? string? vevent?)))
  `(section (@ (class "text-day"))
            (header (h2 (a (@ (href "#" ,header)
                              (class "hidelink"))
                           ,header)))
            ,@(for entry in entries
               (define store-id (list-ref entry 0))
               (define ev (list-ref entry 2))
               (fmt-single-event
                ev `((id ,(html-id ev) "-side")
                     (data-calendar ,(base64encode store-id)))
                fmt-header:
                (lambda body
                  `(a (@ (href "#" ,(html-id ev) "-block" #; (date-link (as-date (prop ev 'DTSTART)))
                               )
                         (class "hidelink"))
                      ,@body))))))



;; Generate a series of top level CSS blocks, setting --color and
;; --complement on any event matching `data-calendar="${base64(calname)}"`.
(define* (calendar-styles calendars optional: (port #f))
  (typecheck calendars (list-of (pair-of string? calendar-data-store?)))
  ;; Specific styles for each calendar.
  (format port "~:{ [data-calendar=\"~a\"] { --color: ~a; --complement: ~a }~%~}"
          (map (lambda (c)
                 (let ((name (base64encode (car c)))
                       (bg-color (and=> (store-color (cdr c)) rgb->hex))
                       (fg-color (and=> (store-color (cdr c))
                                        calculate-fg-color)))
                   (list name (or bg-color 'white) (or fg-color 'black))))
               calendars)))

;; "Physical" block in calendar view
(define* (make-block calendar-id href ev optional: (extra-attributes '()))
  (typecheck calendar-id string?)
  (typecheck href string?)
  (typecheck ev vevent?)
  ;; Should technically be (list-of (pair-of symbol? string?))
  ;; But relaxed since sxml-simple allows basically anything
  (typecheck extra-attributes (list-of (pair-of symbol? any-type)))

  ;; surrounding <a /> element which allows something to happen when an element
  ;; is clicked with JS turned off. Our JS disables this, and handles clicks itself.
  `((a (@ (href "#" ,(html-id ev) "-side")
          (class "hidelink"))
       (vevent-block (@ ,@(assq-merge
                           extra-attributes
                           `((id ,(html-id ev) "-block")
                             (data-calendar ,(base64encode calendar-id))
                             (data-uid ,(output-uid ev))

                             (class "vevent event"
                               ,(when (and (prop% ev 'PARTSTAT)
                                           (eq? 'TENTATIVE (prop1 ev 'PARTSTAT)))
                                  " tentative")
                               ,(when (and (prop% ev 'TRANSP)
                                           (eq? 'TRANSPARENT (prop1 ev 'TRANSP)))
                                  " transparent")
                               ))))
                     ;; Inner div to prevent overflow. Previously "overflow: none"
                     ;; was set on the surounding div, but the popup /needs/ to
                     ;; overflow (for the tabs?).
                     ;; TODO the above comment is no longer valid. Popups are now stored
                     ;; separately from the block.
                     (div (@ (class "event-body"))
                          ,(when (prop% ev 'RRULE)
                             `(span (@ (class "repeating")) "↺"))
                          (span (@ (class "summary")
                                   (data-property "summary"))
                                ,(format-summary ev (prop1 ev 'SUMMARY)))
                          ,(when (prop% ev 'LOCATION)
                             `(span (@ (class "location")
                                       (data-property "location"))
                                    ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                                 (prop1 ev 'LOCATION))))
                          ;; Document symbol when we have text
                          ;; TODO this gets completely misplaced in
                          ;; month view, due to the components being so much smaller.
                          ,(when (prop% ev 'DESCRIPTION)
                             `(span (@ (class "description"))
                                    "🗎")))))))


;; Return a unique identifier for a specific instance of an event.
;; Allows us to reference each instance of a repeating event separately
;; from any other
;; DEPRECATED, either instead switch to (uid or href) + recurrence-id,
;; as a tuple.
(define (output-uid event)
  (prop1 event 'UID)
  #;
  (string-concatenate
   (cons
    (prop1 event 'UID)
    (when (recurring? event)
      ;; TODO this will break if a UID already looks like this...
      ;; Just using a pre-generated unique string would solve it,
      ;; until someone wants to break us. Therefore, we just give
      ;; up for now, until a proper solution can be devised.
      (list "---"
            ;; TODO Will this give us a unique identifier?
            ;; Or can two events share UID along with start time
            (datetime->string
             (as-datetime (or
                           ;; TODO What happens if the parameter RANGE=THISANDFUTURE is set?
                           (prop1 event 'RECURRENCE-ID)
                           (prop1 event 'DTSTART)))
             "~Y-~m-~dT~H:~M:~S"))))))


(define (week-day-select args)
  (define weekdays #(SU MO TU WE TH FR SA))
  `(select (@ ,@args)
     (option "-")
     ,@(map (lambda (w) `(option (@ (value ,(vector-ref weekdays w)))
                            ,(week-day-name w)))
            (weekday-list))))


;;; Templates


;; edit tab of popup
(define (edit-template calendars)
  (typecheck calendars (list-of (pair-of string? calendar-data-store?)))
  `(template
    (@ (id "vevent-edit"))
    (div (@ (class " eventtext edit-tab "))
         (form (@ (class "edit-form"))
               (select (@ (class "calendar-selection"))
                 ;; NOTE flytta "muffarna" utanför
                 (option ,(G_ "- Choose a Calendar -"))
                 ,@(let ((dflt ((@ (vcomponent config) default-calendar))))
                     (map (lambda (calendar)
                            (define name (car calendar))
                            `(option (@ (value ,(base64encode name))
                                        ,@(when (string=? name dflt)
                                            '((selected))))
                                     ,(or (store-displayname (cdr calendar))
                                          (car calendar))))
                          calendars)))
               (input (@ (type "text")
                         (placeholder ,(G_ "Summary"))
                         (name "summary") (required)
                         (data-property "summary")
                                        ; (value ,(prop ev 'SUMMARY))
                         ))

               (div (@ (class "timeinput"))

                    (date-time-input (@ (name "dtstart")
                                         (data-property "dtstart")
                                         ))

                    (date-time-input (@ (name "dtend")
                                         (data-property "dtend")))

                    (div (@ (class "checkboxes"))
                         (input (@ (type "checkbox")
                                   (name "wholeday")
                                   (data-label ,(G_ "Whole day?"))
                                   ))
                         (input (@ (type "checkbox")
                                   (name "has_repeats")
                                   (data-label ,(G_ "Recurring?"))
                                   )))

                    )

               (input (@ (placeholder ,(G_ "Location"))
                         (data-label ,(G_ "Location"))
                         (name "location")
                         (type "text")
                         (data-property "location")
                                        ; (value ,(or (prop ev 'LOCATION) ""))
                         ))

               (textarea (@ (placeholder ,(G_ "Description"))
                            (data-label ,(G_ "Description"))
                            (data-property "description")
                            (name "description"))
                                        ; ,(prop ev 'DESCRIPTION)
                         )

               (input-list
                (@ (name "categories")
                   (data-property "categories")
                   (data-label ,(G_ "Categories")))
                (input (@ (type "text")
                          (placeholder ,(G_ "Category")))))

               ;; TODO This should be a "list" where any field can be edited
               ;; directly. Major thing holding us back currently is that
               ;; <input-list /> doesn't supported advanced inputs
               ;; (div (@ (class "input-list"))
               ;;      (div (@ (class "unit final newfield"))
               ;;           (input (@ (type "text")
               ;;                     (list "known-fields")
               ;;                     (placeholder "Nytt fält")))
               ;;           (select (@ (name "TYPE"))
               ;;             (option (@ (value "TEXT")) "Text"))
               ;;           (span
               ;;            (input (@ (type "text")
               ;;                      (placeholder "Värde"))))))

               ;; (hr)


               (input (@ (type "submit")))
               ))))

;; description in sidebar / tab of popup
;; Template data for <vevent-description />
(define (description-template)
  `(template
    (@ (id "vevent-description"))
    (div (@ (class " vevent eventtext summary-tab " ()))
         (h3 ((span (@ (class "repeating"))
                    "↺")
              (span (@ (class "summary")
                       (data-property "summary")))))
         ;; Tags are populated with sample data.
         ;; This data WILL be replaced or removed by JavaScript.
         (div (div (time (@ (class "dtstart")
                            (data-property "dtstart")
                            (data-fmt "~L~H:~M")
                            (datetime "PLACEHOLDER"))
                         "02:00")
                   "&nbsp;—&nbsp;"
                   (time (@ (class "dtend")
                            (data-property "dtend")
                            (data-fmt "~L~H:~M")
                            (datetime "PLACEHOLDER"))
                         "23:00"))

              (div (@ (class "fields"))
                   (div (b ,(G_ "Location: "))
                        (div (@ (class "location")
                                (data-property "location"))
                             "Alsättersgatan 13"))
                   (div (@ (class "description")
                           (data-property "description"))
                        "With a description")

                   (div (@ (class "categories")
                           (data-property "categories")))

                   ;; TODO attachments

                   ;; (div (@ (class "categories"))
                   ;;      (a (@ (class "category")
                   ;;            (href "/search/?"
                   ;;                  "q=%28member%20%22test%22%20%28or%20%28prop%20event%20%28quote%20CATEGORIES%29%29%20%28quote%20%28%29%29%29%29"))
                   ;;         test))
                   ;; (div (@ (class "rrule"))
                   ;;      "Upprepas "
                   ;;      "varje vecka"
                   ;;      ".")
                   (div (@ (class "last-modified"))
                        ,(G_ "Last Modified") " -"
                                        ; "2021-09-29 19:56"
                        ))))))

(define (vevent-edit-rrule-template)
  `(template
    (@ (id "vevent-edit-rrule"))
    (div (@ (class "eventtext"))
         (h2 ,(G_ "Recurrences"))
         (dl
          (dt ,(G_ "Frequency"))
          (dd (select (@ (name "freq"))
                (option "-")
                ,@(map (lambda (x) `(option (@ (value ,x)) ,(string-titlecase (symbol->string x))))
                       '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))))

          (dt ,(G_ "Until"))
          (dd (date-time-input (@ (name "until"))))

          (dt ,(G_ "Conut"))
          (dd (input (@ (type "number") (name "count") (min 0))))

          (dt ,(G_ "Interval"))
          (dd (input (@ (type "number") (name "interval") ; min and max depend on FREQ
                        )))

          ,@(concatenate
             (map (lambda (pair)
                    (define name (list-ref pair 0))
                    (define pretty-name (list-ref pair 1))
                    (define min (list-ref pair 2))
                    (define max (list-ref pair 3))
                    `((dt ,pretty-name)
                      (dd (input-list (@ (name ,name))
                                      (input (@ (type "number")
                                                (min ,min) (max ,max)))))))
                  `((bysecond ,(G_ "By Second") 0 60)
                    (byminute ,(G_ "By Minute") 0 59)
                    (byhour ,(G_ "By Hour") 0 23)
                    (bymonthday ,(G_ "By Month Day") -31 31) ; except 0
                    (byyearday ,(G_ "By Year Day") -366 366) ; except 0
                    (byweekno ,(G_ "By Week Number") -53 53) ; except 0
                    (bymonth ,(G_ "By Month") 1 12)
                    (bysetpos ,(G_ "By Set Position") -366 366) ; except 0
                    )))

          ;; (dt "By Week Day")
          ;; (dd (input-list (@ (name "byweekday"))
          ;;                 (input (@ (type number)
          ;;                           (min -53) (max 53) ; except 0
          ;;                           ))
          ;;                 ,(week-day-select '())
          ;;                 ))

          (dt ,(G_ "Weekstart"))
          (dd ,(week-day-select '((name "wkst")))))))
  )


;; Based on popup:s output
(define (popup-template)
  `(template
    (@ (id "popup-template"))
    ;; becomes the direct child of <popup-element/>
    (div (@ (class "popup-root window")
            (onclick "event.stopPropagation()"))

         (nav (@ (class "popup-control"))
              (button (@ (class "close-button")
                         ;; Close this popup
                         (title ,(G_ "Close"))
                         (aria-label "Close"))
                      "×")
              (button (@ (class "maximize-button")
                         ;; Make this popup occupy the entire screen
                         (title ,(G_ "Fullscreen"))
                         ;; (aria-label "")
                         )
                      ,(xml-entities "🗖"))
              (button (@ (class "remove-button")
                         ;; Remove/Trash the event this popup represent
                         ;; Think garbage can
                         (title ,(G_ "Remove")))
                      ,(xml-entities "🗑")))

         (tab-group (@ (class "window-body"))
                    (vevent-description
                     (@ (data-label ,(xml-entities "📅")) (data-title ,(G_ "Overview"))
                        (class "vevent")))

                    (vevent-edit
                     (@ (data-label ,(xml-entities "🖊"))
                        (data-title ,(G_ "Edit"))
                        ;; Used by JavaScript to target this tab
                        (data-originaltitle "Edit")))

                    ;; (vevent-edit-rrule
                    ;;  (@ (data-label "↺") (data-title "Upprepningar")))

                    (vevent-changelog
                     (@ (data-label ,(xml-entities "📒"))
                        (data-title ,(G_ "Changelog"))))

                    ,@(when (debug)
                        `((vevent-dl
                           (@ (data-label ,(xml-entities "🐸"))
                              (data-title ,(G_ "Debug"))))))))))

(define-module (calp html vcomponent)
  :use-module (hnh util)
  ;; TODO should we really use path-append here? Path append is
  ;; system-dependant, while URL-paths aren't.
  :use-module ((hnh util path) :select (path-append))
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module ((rnrs io ports) :select (put-bytevector))
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module ((text util) :select (add-enumeration-punctuation))
  :use-module ((web uri-query) :select (encode-query-parameters))
  :use-module ((calp html util) :select (html-id calculate-fg-color))
  :use-module ((calp html config) :select (edit-mode debug))
  :use-module ((crypto) :select (sha256 checksum->string))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module ((vcomponent recurrence) :select (repeating?))
  :use-module ((vcomponent datetime output)
               :select (fmt-time-span
                        format-recurrence-rule
                                      ))
  :use-module (calp util config)
  :use-module ((base64) :select (base64encode))
  :use-module (ice-9 format)
  :use-module (calp translation)
  :use-module (calp html filter)
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


(define (xml-entities s)
  (lambda ()
    (for-each display
     (map (lambda (c) (format #f "&#x~x;" (char->integer c)))
          (string->list s)))))

(define (format-summary ev str)
  ((summary-filter) ev str))

;; NOTE this should have information about context (html/term/...)
;; And then be moved somewhere else.
(define (format-description ev str)
  (catch* (lambda () ((description-filter) ev str))
          (configuration-error
           (lambda (key subr msg args data)
             (format (current-error-port)
                     (G_ "Error retrieving configuration, ~?~%") msg args)))
          (#t ; for errors when running the filter
           (lambda (err . args)
             (warning (G_ "~a on formatting description, ~s") err args)
             str))))

;; TODO replace with propper mimetype parser
(define (mimetype-extension mimetype)
  ((@ (ice-9 match) match) mimetype
    ('() "unknown")
    ('("image" "png") "png")
    ('("image" "jpg") "jpg")
    ('("image" "jpeg") "jpg")
    ('("image" "gif") "gif")
    ))

;; used by search view
(define (compact-event-list list)

  (define calendars
   (delete-duplicates!
    (filter (lambda (x) (eq? 'VCALENDAR (type x)))
            (map parent list))
    eq?))

  (define (summary event)
    `(summary (div (@ (class "summary-line "))
                   (span (@ (class "square")
                            (data-calendar
                             ,(base64encode
                               (or (prop (parent event)
                                         'NAME)
                                   "unknown")))))
                   (time ,(let ((dt (prop event 'DTSTART)))
                            (if (datetime? dt)
                                (datetime->string dt (G_ "~Y-~m-~d ~H:~M"))
                                (date->string dt (G_ "~Y-~m-~d") ))))
                   (a (@ (href ,(date->string (as-date (prop event 'DTSTART)) "/week/~Y-~m-~d.html")))
                      ;; Button for viewing calendar, accompanied by a calendar icon
                      ,(G_ "View") " 📅")
                   (span ,(prop event 'SUMMARY)))))
  (cons
   `(style ,(lambda () (calendar-styles calendars #t)))
   (for event in list
        `(details
          ,(summary event)
          ;; TODO better format
          ,(fmt-single-event event)))))

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
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (prop ev 'X-HNH-FILENAME))
  `(vevent-description
    (@ ,@(assq-merge
          attributes
          `(
            (class ,(when (and (prop ev 'PARTSTAT)
                               (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                      " tentative "))
            (data-uid ,(output-uid ev)))))
    (div (@ (class "vevent eventtext summary-tab"))
         (h3 ,(fmt-header
               (when (prop ev 'RRULE)
                 `(span (@ (class "repeating")) "↺"))
               `(span (@ (class "summary")
                         (data-property "summary"))
                      ,(prop ev 'SUMMARY))))
         (div
          ;; TODO localize this?
          ,(call-with-values (lambda () (fmt-time-span ev))
             (case-lambda [(start)
                           `(div (time (@ (class "dtstart")
                                          (data-property "dtstart")
                                          (data-fmt ,(string-append "~L" start))
                                          (datetime ,(datetime->string
                                                      (as-datetime (prop ev 'DTSTART))
                                                      "~1T~3")))
                                       ,(datetime->string
                                         (as-datetime (prop ev 'DTSTART))
                                         start)))]
                          [(start end)
                           `(div (time (@ (class "dtstart")
                                          (data-property "dtstart")
                                          (data-fmt ,(string-append "~L" start))
                                          (datetime ,(datetime->string
                                                      (as-datetime (prop ev 'DTSTART))
                                                      "~1T~3")))
                                       ,(datetime->string (as-datetime (prop ev 'DTSTART))
                                                          start))
                                 " — "
                                 (time (@ (class "dtend")
                                          (data-property "dtend")
                                          (data-fmt ,(string-append "~L" end))
                                          (datetime ,(datetime->string
                                                      (as-datetime (prop ev 'DTSTART))
                                                      "~1T~3")))
                                       ,(datetime->string (as-datetime (prop ev 'DTEND))
                                                          end)))]))

          (div (@ (class "fields"))
               ,(when (and=> (prop ev 'LOCATION) (negate string-null?))
                  `(div (b ,(G_ "Location: "))
                        (div (@ (class "location") (data-property "location"))
                             ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                          (prop ev 'LOCATION)))))
               ,(awhen (prop ev 'DESCRIPTION)
                       `(div (@ (class "description")
                                (data-property "description"))
                             ,(format-description ev it)))

               ,@(awhen (prop* ev 'ATTACH)
                        ;; attach satisfies @code{vline?}
                        (for attach in it
                             (case (and=> (param attach 'VALUE) (compose string->symbol car))
                               ((BINARY)
                                ;; TODO guess datatype if FMTTYPE is missing
                                (let ((fmt-type (and=> (param attach 'FMTTYPE)
                                                       (lambda (p) (string-split (car p) #\/)))))
                                  ;; TODO other file formats
                                  (cond ((and fmt-type
                                              (not (null? fmt-type))
                                              (string=? "image" (car fmt-type)))
                                         (let* ((chk (-> (value attach)
                                                         sha256
                                                         checksum->string))
                                                (dname (path-append (xdg-runtime-dir)
                                                                    "calp-data" "images"))
                                                (filename (-> dname
                                                              (path-append chk)
                                                              (string-append "." (mimetype-extension fmt-type)))))
                                           (unless (file-exists? filename)
                                             ;; TODO handle tmp directory globaly
                                             (mkdir (dirname dname))
                                             (mkdir dname)
                                             (call-with-output-file filename
                                               (lambda (port) (put-bytevector port (value attach)))))
                                           (let ((link (path-append "/tmpfiles" (string-append chk "." (mimetype-extension fmt-type)))))
                                             `(a (@ (href ,link))
                                                 (img (@ (class "attach")
                                                         (src ,link)))))))
                                        (else `(pre "As of yet unsupported file format" ,fmt-type)))))
                               ((URI)
                                (let ((fmt-type (and=> (param attach 'FMTTYPE)
                                                       (lambda (p) (string-split (car p) #\/)))))
                                  (cond ((and fmt-type
                                              (not (null? fmt-type))
                                              (string=? "image" (car fmt-type)))
                                         `(img (@ (class "attach")
                                                  (src ,(value attach)))))
                                        (else `(a (@ (class "attach")
                                                     (href ,(value attach)))
                                                  ,(value attach))))))

                               ;; Neither BINARY nor URI
                               (else (scm-error 'misc-error "fmt-single-event"
                                                "Unknown attachement type ~s, expected BINARY or UID"
                                                (list (and=> (param attach 'VALUE) car))
                                                #f)))))

               ,(awhen (prop ev 'CATEGORIES)
                       `(div (@ (class "categories"))
                             ,@(map (lambda (c)
                                      `(a (@ (class "category")
                                             ;; TODO centralize search terms
                                             (href
                                              "/search/?"
                                              ,(encode-query-parameters
                                                `((q . (member
                                                        ,(->string c)
                                                        (or (prop event 'CATEGORIES)
                                                            '())))))))
                                          ,c))
                                    it)))

               ,(awhen (prop ev 'RRULE)
                       `(div (@ (class "rrule"))
                             ,@(format-recurrence-rule ev)))

               ,(when (prop ev 'LAST-MODIFIED)
                  `(div (@ (class "last-modified")) ,(G_ "Last modified") " "
                        ,(datetime->string (prop ev 'LAST-MODIFIED)
                                           ;; Last modified datetime
                                           (G_ "~1 ~H:~M")))))

          ))))



;; Single event in side bar (text objects)
(define (fmt-day day)
  (let ((date (car day))
        (events (cdr day)))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date (G_ "~Y-~m-~d"))))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map
                  (lambda (ev)
                    (fmt-single-event
                      ev `((id ,(html-id ev) "-side")
                           (data-calendar ,(base64encode (or (prop (parent ev) 'NAME) "unknown"))))
                      fmt-header:
                      (lambda body
                        `(a (@ (href "#" ,(html-id ev) "-block" #; (date-link (as-date (prop ev 'DTSTART)))
                                     )
                               (class "hidelink"))
                            ,@body))))
                  (stream-filter
                   (lambda (ev)
                     ;; If start was an earlier day
                     ;; This removes all descriptions from
                     ;; events for previous days,
                     ;; solving duplicates.
                     (date/-time<=? date (prop ev 'DTSTART)))
                   events))))))


;; Specific styles for each calendar.
(define* (calendar-styles calendars optional: (port #f))
  (format port "~:{ [data-calendar=\"~a\"] { --color: ~a; --complement: ~a }~%~}"
          (map (lambda (c)
                 (let ((name (base64encode (prop c 'NAME)))
                       (bg-color (prop c 'COLOR))
                       (fg-color (and=> (prop c 'COLOR)
                                        calculate-fg-color)))
                   (list name (or bg-color 'white) (or fg-color 'black))))
               calendars)))

;; "Physical" block in calendar view
(define* (make-block ev optional: (extra-attributes '()))

  ;; surrounding <a /> element which allows something to happen when an element
  ;; is clicked with JS turned off. Our JS disables this, and handles clicks itself.
  `((a (@ (href "#" ,(html-id ev) "-side")
          (class "hidelink"))
       (vevent-block (@ ,@(assq-merge
                           extra-attributes
                           `((id ,(html-id ev) "-block")
                             (data-calendar ,(base64encode (or (prop (parent ev) 'NAME) "unknown")))
                             (data-uid ,(output-uid ev))

                             (class "vevent event"
                               ,(when (and (prop ev 'PARTSTAT)
                                           (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                                  " tentative")
                               ,(when (and (prop ev 'TRANSP)
                                           (eq? 'TRANSPARENT (prop ev 'TRANSP)))
                                  " transparent")
                               ))))
                     ;; Inner div to prevent overflow. Previously "overflow: none"
                     ;; was set on the surounding div, but the popup /needs/ to
                     ;; overflow (for the tabs?).
                     ;; TODO the above comment is no longer valid. Popups are now stored
                     ;; separately from the block.
                     (div (@ (class "event-body"))
                          ,(when (prop ev 'RRULE)
                             `(span (@ (class "repeating")) "↺"))
                          (span (@ (class "summary")
                                   (data-property "summary"))
                                ,(format-summary  ev (prop ev 'SUMMARY)))
                          ,(when (prop ev 'LOCATION)
                             `(span (@ (class "location")
                                       (data-property "location"))
                                    ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                                 (prop ev 'LOCATION))))
                          ;; Document symbol when we have text
                          ,(when (and=> (prop ev 'DESCRIPTION) (negate string-null?))
                             `(span (@ (class "description"))
                                    "🗎")))))))


;; TODO possibly unused?
(define (repeat-info event)
  `(div (@ (class "eventtext"))
        (h2 ,(G_ "Recurrences"))
        (table (@ (class "recur-components"))
               ,@((@@ (vcomponent recurrence internal) map-fields)
                  (lambda (key value)
                    `(tr (@ (class ,key)) (th ,key)
                         (td
                          ;; TODO Should these date string be translated?
                          ,(case key
                             ((wkst) (week-day-name value))
                             ((until) (if (date? value)
                                          (date->string value)
                                          (datetime->string value)))
                             ((byday) (add-enumeration-punctuation
                                       (map (lambda (pair)
                                              (string-append
                                               (if (car pair)
                                                   (format #f "~a " (car pair))
                                                   "")
                                               (week-day-name (cdr pair))))
                                            value)))
                             (else (->string value))))))
                  (prop event 'RRULE)))))


;; Return a unique identifier for a specific instance of an event.
;; Allows us to reference each instance of a repeating event separately
;; from any other
(define (output-uid event)
  (string-concatenate
   (cons
    (prop event 'UID)
    (when (repeating? event)
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
                           (prop event 'RECURRENCE-ID)
                           (prop event 'DTSTART)))
             "~Y-~m-~dT~H:~M:~S"))))))


(define (week-day-select args)
  `(select (@ ,@args)
     (option "-")
     ,@(map (lambda (x) `(option (@ (value ,(car x))) ,(cadr x)))
            ;; TODO translate
            '((MO "Monday")
              (TU "Tuesday")
              (WE "Wednesday")
              (TH "Thursday")
              (FR "Friday")
              (SA "Saturday")
              (SU "Sunday")))))


;;; Templates


;; edit tab of popup
(define (edit-template calendars)
  `(template
    (@ (id "vevent-edit"))
    (div (@ (class " eventtext edit-tab "))
         (form (@ (class "edit-form"))
               (select (@ (class "calendar-selection"))
                 ;; NOTE flytta "muffarna" utanför
                 (option ,(G_ "- Choose a Calendar -"))
                 ,@(let ((dflt ((@ (vcomponent) default-calendar))))
                     (map (lambda (calendar)
                            (define name (prop calendar 'NAME))
                            `(option (@ (value ,(base64encode name))
                                        ,@(when (string=? name dflt)
                                            '((selected))))
                                     ,name))
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
         ;; TODO should't the time tags contain something?
         (div (div (time (@ (class "dtstart")
                            (data-property "dtstart")
                            (data-fmt "~L~H:~M")
                            (datetime ; "2021-09-29T19:56:46"
                             ))
                                        ; "19:56"
                         )
                   "\xa0—\xa0"
                   (time (@ (class "dtend")
                            (data-property "dtend")
                            (data-fmt "~L~H:~M")
                            (datetime ; "2021-09-29T19:56:46"
                             ))
                                        ; "20:56"
                         ))
              (div (@ (class "fields"))
                   (div (b ,(G_ "Location: "))
                        (div (@ (class "location")
                                (data-property "location"))
                                        ; "Alsättersgatan 13"
                             ))
                   (div (@ (class "description")
                           (data-property "description"))
                                        ; "With a description"
                        )

                   (div (@ (class "categories")
                           (data-property "categories")))
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

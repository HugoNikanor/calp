(define-module (calp html vcomponent)
  :use-module (calp util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module ((rnrs io ports) :select (put-bytevector))
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module ((text util) :select (add-enumeration-punctuation))
  :use-module ((web uri-query) :select (encode-query-parameters))
  :use-module (calp html util)
  :use-module ((calp html config) :select (edit-mode debug))
  :use-module ((calp html components) :select (btn tabset form with-label))
  :use-module ((calp util color) :select (calculate-fg-color))
  :use-module ((crypto) :select (sha256 checksum->string))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module ((vcomponent recurrence) :select (repeating?))
  :use-module ((vcomponent datetime output)
               :select (fmt-time-span
                        format-description
                        format-summary
                        format-recurrence-rule
                                      ))
  :use-module ((calp util config) :select (get-config))
  :use-module ((base64) :select (base64encode))
  )

;; used by search view
(define-public (compact-event-list list)

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
                                (datetime->string dt "~Y-~m-~d ~H:~M")
                                (date->string dt "~Y-~m-~d" ))))
                   (a (@ (href ,(date->string (as-date (prop event 'DTSTART)) "/week/~Y-~m-~d.html")))
                      "View 📅")
                   (span ,(prop event 'SUMMARY)))))
  (cons
   (calendar-styles calendars)
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
(define*-public (fmt-single-event ev
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
                  `(div (b "Plats: ")
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
                             (if (and=> (param attach 'VALUE)
                                        (lambda (p) (string=? "BINARY" (car p))))
                                 ;; Binary data
                                 ;; TODO guess datatype if FMTTYPE is missing
                                 (awhen (and=> (param attach 'FMTTYPE)
                                               (lambda (it) (string-split
                                                        (car it) #\/)))
                                        ;; TODO other file formats
                                        (when (string=? "image" (car it))
                                          (let* ((chk (-> (value attach)
                                                          sha256
                                                          checksum->string))
                                                 (dname
                                                  (path-append (xdg-runtime-dir)
                                                               "calp-data" "images"))
                                                 (filename (-> dname
                                                               (path-append chk)
                                                               ;; TODO second part of mimetypes
                                                               ;; doesn't always result in a valid
                                                               ;; file extension.
                                                               ;; Take a look in mime.types.
                                                               (string-append "." (cadr it)))))
                                            (unless (file-exists? filename)
                                              ;; TODO handle tmp directory globaly
                                              (mkdir (dirname dname))
                                              (mkdir dname)
                                              (call-with-output-file filename
                                                (lambda (port)
                                                  (put-bytevector port (value attach)))))
                                            (let ((link (path-append
                                                         "/tmpfiles"
                                                         ;; TODO better mimetype to extension
                                                         (string-append chk "." (cadr it)))))
                                              `(a (@ (href ,link))
                                                  (img (@ (class "attach")
                                                          (src ,link))))))))
                                 ;; URI
                                 (cond ((and=> (param attach 'FMTTYPE)
                                               (lambda (p) (string=? (car p) "image" 0 5)))
                                        `(img (@ (class "attach")
                                                 (src ,(value attach)))))
                                       (else `(a (@ (class "attach")
                                                    (href ,(value attach)))
                                                 ,(value attach)))))))

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
                  `(div (@ (class "last-modified")) "Senast ändrad "
                        ,(datetime->string (prop ev 'LAST-MODIFIED) "~1 ~H:~M"))))

          ))))



;; Single event in side bar (text objects)
(define-public (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map
                  (lambda (ev)
                    (fmt-single-event
                      ev `((id ,(html-id ev))
                           (data-calendar ,(base64encode (or (prop (parent ev) 'NAME) "unknown"))))
                      fmt-header:
                      (lambda body
                        `(a (@ (href "#" ,(html-id ev) #; (date-link (as-date (prop ev 'DTSTART)))
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
;; TODO only emit the CSS here, requiring the caller to handle the context,
;; since that would allow us to use this in other contexts.
(define-public (calendar-styles calendars)
  `(style
       ,(lambda () (format #t "~:{ [data-calendar=\"~a\"] { --color: ~a; --complement: ~a }~%~}"
                      (map (lambda (c)
                             (let* ((name (base64encode (prop c 'NAME)))
                                    (bg-color (prop c 'COLOR))
                                    (fg-color (and=> (prop c 'COLOR)
                                                     calculate-fg-color)))
                               (list name (or bg-color 'white) (or fg-color 'black))))
                           calendars)))))

;; "Physical" block in calendar view
(define*-public (make-block ev optional: (extra-attributes '()))

  ;; surrounding <a /> element which allows something to happen when an element
  ;; is clicked with JS turned off. Our JS disables this, and handles clicks itself.
  `((a (@ (href "#" ,(html-id ev))
          (class "hidelink"))
       (vevent-block (@ ,@(assq-merge
                           extra-attributes
                           `((id ,(html-id ev))
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
        (h2 "Upprepningar")
        (table (@ (class "recur-components"))
               ,@((@@ (vcomponent recurrence internal) map-fields)
                  (lambda (key value)
                    `(tr (@ (class ,key)) (th ,key)
                         (td
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
(define-public (output-uid event)
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
            '((MO "Monday")
              (TU "Tuesday")
              (WE "Wednesday")
              (TH "Thursday")
              (FR "Friday")
              (SA "Saturday")
              (SU "Sunday")))))


;;; Templates


;; edit tab of popup
(define-public (edit-template calendars)
  `(template
    (@ (id "vevent-edit"))
    (div (@ (class " eventtext edit-tab "))
         (form (@ (class "edit-form"))
               (select (@ (class "calendar-selection"))
                 (option "- Choose a Calendar -")
                 ,@(let ((dflt (get-config 'default-calendar)))
                     (map (lambda (calendar)
                            (define name (prop calendar 'NAME))
                            `(option (@ (value ,(base64encode name))
                                        ,@(when (string=? name dflt)
                                            '((selected))))
                                     ,name))
                          calendars)))
               (h3 (input (@ (type "text")
                             (placeholder "Sammanfattning")
                             (name "summary") (required)
                             (data-property "summary")
                                        ; (value ,(prop ev 'SUMMARY))
                             )))

               (div (@ (class "timeinput"))

                    ,@(with-label
                       "Starttid"
                       '(date-time-input (@ (name "dtstart")
                                            (data-property "dtstart")
                                            )))

                    ,@(with-label
                       "Sluttid"
                       '(date-time-input (@ (name "dtend")
                                            (data-property "dtend"))))

                    (div (@ (class "checkboxes"))
                         ,@(with-label
                            "Heldag?"
                            `(input (@ (type "checkbox")
                                       (name "wholeday")
                                       )))
                         ,@(with-label
                            "Upprepande?"
                            `(input (@ (type "checkbox")
                                       (name "has_repeats")
                                       ))))

                    )

               ,@(with-label
                  "Plats"
                  `(input (@ (placeholder "Plats")
                             (name "location")
                             (type "text")
                             (data-property "location")
                                        ; (value ,(or (prop ev 'LOCATION) ""))
                             )))

               ,@(with-label
                  "Beskrivning"
                  `(textarea (@ (placeholder "Beskrivning")
                                (data-property "description")
                                (name "description"))
                                        ; ,(prop ev 'DESCRIPTION)
                             ))

               ,@(with-label
                  "Kategorier"
                  `(input-list
                    (@ (name "categories")
                       (data-property "categories"))
                    (input (@ (type "text")
                              (placeholder "Kattegori")))))

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
(define-public (description-template)
  '(template
    (@ (id "vevent-description"))
    (div (@ (class " vevent eventtext summary-tab " ()))
         (h3 ((span (@ (class "repeating"))
                    "↺")
              (span (@ (class "summary")
                       (data-property "summary")))))
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
                   (div (b "Plats: ")
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
                        "Senast ändrad -"
                                        ; "2021-09-29 19:56"
                        ))))))

(define-public (vevent-edit-rrule-template)
  `(template
    (@ (id "vevent-edit-rrule"))
    (div (@ (class "eventtext"))
         (h2 "Upprepningar")
         (dl
          (dt "Frequency")
          (dd (select (@ (name "freq"))
                (option "-")
                ,@(map (lambda (x) `(option (@ (value ,x)) ,(string-titlecase (symbol->string x))))
                       '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))))

          (dt "Until")
          (dd (date-time-input (@ (name "until"))))

          (dt "Conut")
          (dd (input (@ (type "number") (name "count") (min 0))))

          (dt "Interval")
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
                  '((bysecond "By Second" 0 60)
                    (byminute "By Minute" 0 59)
                    (byhour "By Hour" 0 23)
                    (bymonthday "By Month Day" -31 31) ; except 0
                    (byyearday "By Year Day" -366 366) ; except 0
                    (byweekno "By Week Number" -53 53) ; except 0
                    (bymonth "By Month" 1 12)
                    (bysetpos "By Set Position" -366 366) ; except 0
                    )))

          ;; (dt "By Week Day")
          ;; (dd (input-list (@ (name "byweekday"))
          ;;                 (input (@ (type number)
          ;;                           (min -53) (max 53) ; except 0
          ;;                           ))
          ;;                 ,(week-day-select '())
          ;;                 ))

          (dt "Weekstart")
          (dd ,(week-day-select '((name "wkst")))))))
  )


;; Based on popup:s output
(define-public (popup-template)
  `(template
    (@ (id "popup-template"))
    ;; becomes the direct child of <popup-element/>
    (div (@ (class "popup-root window")
            (onclick "event.stopPropagation()"))

         (nav (@ (class "popup-control"))
              (button (@ (class "close-button")
                         (title "Stäng")
                         (aria-label "Close"))
                      "×")
              (button (@ (class "maximize-button")
                         (title "Fullskärm")
                         ;; (aria-label "")
                         )
                      "🗖")
              (button (@ (class "remove-button")
                         (title "Ta Bort"))
                      "🗑"))

         (tab-group (@ (class "window-body"))
                    (vevent-description
                     (@ (data-label "📅") (data-title "Översikt")
                        (class "vevent")))

                    (vevent-edit
                     (@ (data-label "🖊") (data-title "Redigera")))

                    ;; (vevent-edit-rrule
                    ;;  (@ (data-label "↺") (data-title "Upprepningar")))

                    (vevent-changelog
                     (@ (data-label "📒") (date-title "Changelog")))

                    ,@(when (debug)
                        '((vevent-dl
                           (@ (data-label "🐸") (data-title "Debug")))))))))

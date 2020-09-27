(define-module (calp html vcomponent)
  :use-module (calp util)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (datetime)
  :use-module ((text util) :select (add-enumeration-punctuation))
  :use-module (calp html util)
  :use-module ((calp html config) :select (edit-mode))
  :use-module ((calp html components) :select (btn tabset form with-label))
  :use-module ((calp util color) :select (calculate-fg-color))
  :use-module ((vcomponent datetime output)
               :select (fmt-time-span
                        format-description
                        format-summary
                        format-recurrence-rule
                                      ))
  )

(define-public (compact-event-list list)

  (define calendars
   (delete-duplicates!
    (filter (lambda (x) (eq? 'VCALENDAR (type x)))
            (map parent list))
    eq?))

  (define (summary event)
    `(summary (div (@ (class "summary-line "))
                   (span (@ (class "square CAL_"
                              ,(html-attr
                                (or (prop (parent event)
                                          'NAME)
                                    "unknown")))))
                   (time ,(let ((dt (prop event 'DTSTART)))
                            (if (datetime? dt)
                                (datetime->string dt "~Y-~m-~d ~H:~M")
                                (date->string dt "~Y-~m-~d" ))))
                   (span ,(prop event 'SUMMARY)))))
  (cons
   (calendar-styles calendars)
   (for event in list
        `(details
          ,(summary event)
          ;; TODO better format, add show in calendar button
          ,(fmt-single-event event)))))

;; Format event as text.
;; Used in
;; - sidebar
;; - popup overwiew tab
;; - search result (event details)
(define*-public (fmt-single-event ev
                                  optional: (attributes '())
                                  key: (fmt-header list))
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (prop ev 'X-HNH-FILENAME))
  `(div (@ ,@(assq-merge
              attributes
              `((class " eventtext "
                  ,(when (and (prop ev 'PARTSTAT)
                              (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                     " tentative ")))))
        (h3 ,(fmt-header
              (when (prop ev 'RRULE)
                `(span (@ (class "repeating")) "↺"))
              `(span (@ (class "summary")) ,(prop ev 'SUMMARY))))
        (div
         ,(call-with-values (lambda () (fmt-time-span ev))
            (case-lambda [(start)
                          `(div (time (@ (class "dtstart")
                                         (data-fmt ,(string-append "~L" start))
                                         (datetime ,(datetime->string
                                                     (as-datetime (prop ev 'DTSTART))
                                                     "~1T~3")))
                                      ,(datetime->string
                                        (as-datetime (prop ev 'DTSTART))
                                        start)))]
                         [(start end)
                          `(div (time (@ (class "dtstart")
                                         (data-fmt ,(string-append "~L" start))
                                         (datetime ,(datetime->string
                                                     (as-datetime (prop ev 'DTSTART))
                                                     "~1T~3")))
                                      ,(datetime->string (as-datetime (prop ev 'DTSTART))
                                                         start))
                                " — "
                                (time (@ (class "dtend")
                                         (data-fmt ,(string-append "~L" end))
                                         (datetime ,(datetime->string
                                                     (as-datetime (prop ev 'DTSTART))
                                                     "~1T~3")))
                                      ,(datetime->string (as-datetime (prop ev 'DTEND))
                                                         end)))]))
         (div (@ (class "fields"))
          ,(when (and=> (prop ev 'LOCATION) (negate string-null?))
             `(div (b "Plats: ")
                   (div (@ (class "location"))
                        ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                     (prop ev 'LOCATION)))))
          ,(awhen (prop ev 'DESCRIPTION)
                  `(div (@ (class "description"))
                         ,(format-description ev it)))

          ,(awhen (prop ev 'CATEGORIES)
                  `(div (@ (class "categories"))
                        ,@(map (lambda (c)
                                 `(a (@ (class "category")
                                        ;; TODO centralize search terms
                                        ;; TODO propper stringifycation of sexp
                                        (href ,(format #f "/search/?q=%28member+%22~a%22%0D%0A++%28or+%28prop+event+%27CATEGORIES%29+%27%28%29%29%0D%0A"
                                                       c)))
                                     ,c))
                               it)))
          ,(awhen (prop ev 'RRULE)
                  `(div (@ (class "rrule"))
                        ,@(format-recurrence-rule ev)))

          ,(when (prop ev 'LAST-MODIFIED)
             `(div (@ (class "last-modified")) "Senast ändrad "
                   ,(datetime->string (prop ev 'LAST-MODIFIED) "~1 ~H:~M"))))

         )))

(define*-public (fmt-for-edit ev
                              optional: (attributes '())
                              key: (fmt-header list))
  `(div (@ (class " eventtext "))
        (div (@ (class "edit-form"))
             (h3 (input (@ (type "text") (class "summary")
                           (placeholder "Sammanfattning")
                           (name "summary") (required)
                           (value ,(prop ev 'SUMMARY)))))

             ,@(with-label "Heldag" `(input (@ (name "wholeday") (type "checkbox"))))

             ,@(let ((start (prop ev 'DTSTART)))
                 (with-label "Start"
                             `(div (input (@ (type "date")
                                             (name "dtstart-date")
                                             (value ,(date->string (as-date start)))))
                                   (input (@ ,@(when (date? start)
                                                 '((style "display:none")))
                                             (type "time")
                                             (name "dtstart-end")
                                             (value ,(time->string (as-time start))))))))
             ,@(let ((end (prop ev 'DTEND)))
                 (with-label "Slut"
                             `(div (input (@ (type "date")
                                             (name "dtend-date")
                                             ,@(when end `((value ,(date->string (as-date end)))))))
                                   (input (@ ,@(when (date? end)
                                                 '((style "display:none")))
                                             (type "time")
                                             (name "dtend-time")
                                             ,@(when end `((value ,(time->string (as-time end)))))
                                             )))))

             ,@(with-label
                "Plats"
                `(input (@ (placeholder "Plats")
                           (name "location")
                           (type "text")
                           (value ,(or (prop ev 'LOCATION) "")))))

             ,@(with-label
                "Beskrivning"
                `(textarea (@ (placeholder "Beskrivning")
                              (name "description"))
                           ,(prop ev 'DESCRIPTION)))

             ,@(with-label
                "Kategorier"
                (awhen (prop ev 'CATEGORIES)
                       (map (lambda (c) `(button (@ (class "category")) ,c))
                            it))

                `(input (@ (class "category")
                           (type "text")
                           (placeholder "category"))))
             )))


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
                           (class "CAL_" ,(html-attr (or (prop (parent ev) 'NAME) "unknown"))))
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


(define-public (calendar-styles calendars)
  `(style
     ,(format #f "~:{.CAL_~a { --color: ~a; --complement: ~a }~%~}"
              (map (lambda (c)
                     (let* ((name (html-attr (prop c 'NAME)))
                            (bg-color (prop c 'COLOR))
                            (fg-color (and=> (prop c 'COLOR)
                                             calculate-fg-color)))
                       (list name (or bg-color 'white) (or fg-color 'black))))
                   calendars))))

;; "Physical" block in calendar view
(define*-public (make-block ev optional: (extra-attributes '()))

  `((a (@ (href "#" ,(html-id ev))
          (class "hidelink"))
       (div (@ ,@(assq-merge
                  extra-attributes
                  `((id ,(html-id ev))
                    (data-calendar ,(html-attr (or (prop (parent ev) 'NAME) "unknown")))
                    (class "event CAL_" ,(html-attr (or (prop (parent ev) 'NAME)
                                                        "unknown"))
                      ,(when (and (prop ev 'PARTSTAT)
                                  (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                         " tentative")
                      ,(when (and (prop ev 'TRANSP)
                                  (eq? 'TRANSPARENT (prop ev 'TRANSP)))
                         " transparent")
                      )
                    (onclick "toggle_popup('popup' + this.id)")
                    )))
            ;; Inner div to prevent overflow. Previously "overflow: none"
            ;; was set on the surounding div, but the popup /needs/ to
            ;; overflow (for the tabs?).
            (div (@ (class "event-body"))
             ,(when (prop ev 'RRULE)
                `(span (@ (class "repeating")) "↺"))
             (span (@ (class "summary"))
                   ,(format-summary  ev (prop ev 'SUMMARY)))
             ,(when (prop ev 'LOCATION)
                `(span (@ (class "location"))
                       ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                    (prop ev 'LOCATION)))))
            (div (@ (style "display:none !important;"))
                 ,((@ (vcomponent xcal output) ns-wrap)
                   ((@ (vcomponent xcal output) vcomponent->sxcal)
                    ev)))))))


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


(define-public (popup ev id)
  `(div (@ (id ,id) (class "popup-container CAL_" 
                           ,(html-attr (or (prop (parent ev) 'NAME)
                                           "unknown"))) 
           (onclick "event.stopPropagation()"))
        ;; TODO all (?) code uses .popup-container as the popup, while .popup sits and does nothing.
        ;; Do something about this?
        (div (@ (class "popup"))
             (nav (@ (class "popup-control"))
                  ,(btn "×"
                        title: "Stäng"
                        onclick: "close_popup(document.getElementById(this.closest('.popup-container').id))"
                        class: '("close-tooltip"))
                  ,(when (edit-mode)
                     (list
                      (btn "🖊️"
                           title: "Redigera"
                           onclick: "place_in_edit_mode(document.getElementById(this.closest('.popup-container').id.substr(5)))")
                      (btn "🗑"
                           title: "Ta bort"
                           onclick: "remove_event(document.getElementById(this.closest('.popup-container').id.substr(5)))"))))

             ,(tabset
                `(("📅" title: "Översikt"
                   ,(fmt-single-event ev))

                  ("📅" title: "Redigera"
                   ,(fmt-for-edit ev))

                  ("⤓" title: "Nedladdning"
                   (div (@ (class "eventtext") (style "font-family:sans"))
                        (h2 "Ladda ner")
                        (ul (li (a (@ (href "/calendar/" ,(prop ev 'UID) ".ics"))
                                   "som iCal"))
                            (li (a (@ (href "/calendar/" ,(prop ev 'UID) ".xcs"))
                                   "som xCal")))))

                  ,@(when (prop ev 'RRULE)
                      `(("↺" title: "Upprepningar" class: "repeating"
                         ,(repeat-info ev)))))))))

(define-module (calp html vcomponent)
  :use-module (calp util)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (datetime)
  :use-module ((text util) :select (add-enumeration-punctuation))
  :use-module ((web uri-query) :select (encode-query-parameters))
  :use-module (calp html util)
  :use-module ((calp html config) :select (edit-mode debug))
  :use-module ((calp html components) :select (btn tabset form with-label))
  :use-module ((calp util color) :select (calculate-fg-color))
  :use-module ((vcomponent recurrence internal) :prefix #{rrule:}#)
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
(define*-public (fmt-single-event ev
                                  optional: (attributes '())
                                  key: (fmt-header list))
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (prop ev 'X-HNH-FILENAME))
  `(div (@ ,@(assq-merge
              attributes
              `((data-bindby "bind_view")
                (class " eventtext summary-tab "
                  ,(when (and (prop ev 'PARTSTAT)
                              (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                     " tentative ")))))
        (h3 ,(fmt-header
              (when (prop ev 'RRULE)
                `(span (@ (class "repeating")) "↺"))
              `(span (@ (class "bind summary")
                        (data-property "summary"))
                     ,(prop ev 'SUMMARY))))
        (div
         ,(call-with-values (lambda () (fmt-time-span ev))
            (case-lambda [(start)
                          `(div (time (@ (class "bind dtstart")
                                         (data-property "dtstart")
                                         (data-fmt ,(string-append "~L" start))
                                         (datetime ,(datetime->string
                                                     (as-datetime (prop ev 'DTSTART))
                                                     "~1T~3")))
                                      ,(datetime->string
                                        (as-datetime (prop ev 'DTSTART))
                                        start)))]
                         [(start end)
                          `(div (time (@ (class "bind dtstart")
                                         (data-property "dtstart")
                                         (data-fmt ,(string-append "~L" start))
                                         (datetime ,(datetime->string
                                                     (as-datetime (prop ev 'DTSTART))
                                                     "~1T~3")))
                                      ,(datetime->string (as-datetime (prop ev 'DTSTART))
                                                         start))
                                " — "
                                (time (@ (class "bind dtend")
                                         (data-property "dtend")
                                         (data-fmt ,(string-append "~L" end))
                                         (datetime ,(datetime->string
                                                     (as-datetime (prop ev 'DTSTART))
                                                     "~1T~3")))
                                      ,(datetime->string (as-datetime (prop ev 'DTEND))
                                                         end)))]))

         ;; TODO add optional fields when added in frontend
         ;; Possibly by always having them here, just hidden.

         (div (@ (class "fields"))
          ,(when (and=> (prop ev 'LOCATION) (negate string-null?))
             `(div (b "Plats: ")
                   (div (@ (class "bind location") (data-property "location"))
                        ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                     (prop ev 'LOCATION)))))
          ,(awhen (prop ev 'DESCRIPTION)
                  `(div (@ (class "bind description")
                           (data-property "description"))
                         ,(format-description ev it)))

          ;; TODO add bind once I figure out how to bind lists
          ,(awhen (prop ev 'CATEGORIES)
                  `(div (@ (class "categories"))
                        ,@(map (lambda (c)
                                 `(a (@ (class "category")
                                        ;; TODO centralize search terms
                                        (href
                                         "/search/?"
                                         ,(encode-query-parameters
                                           `((q . (member
                                                   ,(->quoted-string c)
                                                   (or (prop event 'CATEGORIES)
                                                       '())))))))
                                     ,c))
                               it)))

          ;; TODO bind
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
  `(div (@ (class " eventtext edit-tab ")
           (data-bindby "bind_edit"))
        (form (@ (class "edit-form"))
              (div (@ (class "dropdown-goes-here")))
              (h3 (input (@ (type "text")
                            (placeholder "Sammanfattning")
                            (name "summary") (required)
                            (class "bind") (data-property "summary")
                            (value ,(prop ev 'SUMMARY)))))

              ,(let ((start (prop ev 'DTSTART))
                     (end (prop ev 'DTEND)))
                 `(div (@ (class "timeinput"))

                       (input (@ (type "date")
                                 (name "dtstart-date")
                                 (style "grid-column:1;grid-row:2")
                                 (class "bind")
                                 (data-property "--dtstart-date")
                                 (value ,(date->string (as-date start)))))

                       (input (@ (type "date")
                                 (name "dtend-date")
                                 (style "grid-column:1;grid-row:3")
                                 (class "bind")
                                 (data-property "--dtend-date")
                                 ,@(when end `((value ,(date->string (as-date end)))))))

                       ,@(with-label
                          "Heldag?"
                          `(input (@ (type "checkbox")
                                     (class "bind")
                                     (data-bindby "bind_wholeday")
                                     (style "display:none")
                                     (name "wholeday"))))

                       (input (@ (type "time")
                                 (name "dtstart-time")
                                 (class "bind")
                                 (data-property "--dtstart-time")
                                 (style "grid-column:3;grid-row:2;"
                                   ,(when (date? start) "display:none"))
                                 (value ,(time->string (as-time start)))))

                       (input (@ (type "time")
                                 (name "dtend-time")
                                 (class "bind")
                                 (data-property "--dtend-time")
                                 (style "grid-column:3;grid-row:3;"
                                   ,(when (date? end) "display:none"))
                                 ,@(when end `((value ,(time->string (as-time end)))))
                                 ))))

              ,@(with-label
                 "Plats"
                 `(input (@ (placeholder "Plats")
                            (name "location")
                            (type "text")
                            (class "bind") (data-property "location")
                            (value ,(or (prop ev 'LOCATION) "")))))

              ,@(with-label
                 "Beskrivning"
                 `(textarea (@ (placeholder "Beskrivning")
                               (class "bind") (data-property "description")
                               (name "description"))
                            ,(prop ev 'DESCRIPTION)))

              ,@(with-label
                 "Kategorier"
                 ;; It would be better if these input-list's worked on the same
                 ;; class=bind system as the fields above. The problem with that
                 ;; is however that each input-list requires different search
                 ;; and join procedures. Currently this is bound in the JS, see
                 ;; [CATEGORIES_BIND].
                 ;; It matches on ".input-list[data-property='categories']".
                 `(div (@ (class "input-list")
                          (data-property "categories"))
                       ,@(awhen (prop ev 'CATEGORIES)
                                (map (lambda (c)
                                       `(input (@ (size 2)
                                                  (class "unit")
                                                  (value ,c))))
                                     it))

                       (input (@ (class "unit final")
                                 (size 2)
                                 (type "text")
                                 ))))

              (hr)

              ;; For custom user fields
              ;; TODO these are currently not bound to anything, so entering data
              ;; here does nothing. Bigest hurdle to overcome is supporting arbitrary
              ;; fields which will come and go in the JavaScript.
              ;; TODO also, all (most? maybe not LAST-MODIFIED) remaining properties
              ;; should be exposed here.
              (div (@ (class "input-list"))
               (div (@ (class "unit final newfield"))
                    (input (@ (type "text")
                              (list "known-fields")
                              (placeholder "Nytt fält")))
                    (select (@ (name "TYPE"))
                      (option (@ (value "TEXT")) "Text"))
                    (span
                     (input (@ (type "text")
                               (placeholder "Värde"))))))

              (hr)


              (input (@ (type "submit")))
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
                    ;; (data-bindon "bind_view")
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
             (span (@ (class "bind summary")
                      (data-property "summary"))
                   ,(format-summary  ev (prop ev 'SUMMARY)))
             ,(when (prop ev 'LOCATION)
                `(span (@ (class "bind location")
                          (data-property "location"))
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

;; TODO bind this into the xcal
(define (editable-repeat-info event)
  `(div (@ (class "eventtext"))
        (h2 "Upprepningar")
        ,@(when (debug)
            '((button (@ (style "position:absolute;right:1ex;top:1ex")
                         (onclick "console.log(event_from_popup(this.closest('.popup-container')).properties.rrule.asJcal());"))
                      "js")))
        (table (@ (class "recur-components bind")
                  (name "rrule")
                  (data-bindby "bind_recur"))
               ,@(map ; (@@ (vcomponent recurrence internal) map-fields)
                  (lambda (key )
                    `(tr (@ (class ,key)) (th ,key)
                         (td
                          ,(case key
                             ((freq)
                              `(select (@ (class "bind-rr") (name "freq"))
                                 (option "-")
                                 ,@(map (lambda (x) `(option (@ (value ,x)
                                                           ,@(awhen (prop event 'RRULE)
                                                                    (awhen (rrule:freq it)
                                                                           (awhen (eq? it x)
                                                                                  '((selected))))))
                                                        ,(string-titlecase
                                                          (symbol->string x))))
                                        '(SECONDLY MINUTELY HOURLY
                                                   DAILY WEEKLY
                                                   MONTHLY YEARLY))))
                             ((until)
                              (if (date? (prop event 'DTSTART))
                                  `(input (@ (type "date")
                                             (name "until")
                                             (class "bind-rr")
                                             (value ,(awhen (prop event 'RRULE)
                                                            (awhen (rrule:until it)
                                                                   (date->string it))))))
                                  `(span (@ (class "bind-rr date-time")
                                            (name "until"))
                                    (input (@ (type "date")
                                              (value ,(awhen (prop event 'RRULE)
                                                             (awhen (rrule:until it)
                                                                    (date->string
                                                                     (as-date it)))))))
                                    (input (@ (type "time")
                                              (value ,(awhen (prop event 'RRULE)
                                                             (awhen (rrule:until it)
                                                                    (time->string
                                                                     (as-time it))))))))))
                             ((count)
                              `(input (@ (type number) (min 0) (size 4)
                                         (value ,(awhen (prop event 'RRULE)
                                                        (or (rrule:count it) "")))
                                         (name "count")
                                         (class "bind-rr")
                                         )))
                             ((interval)
                              `(input (@ (type number) (min 0) (size 4)
                                         (value ,(awhen (prop event 'RRULE)
                                                        (or (rrule:interval it) "")))
                                         (name "interval")
                                         (class "bind-rr"))))
                             ((wkst)
                              `(select (@ (name "wkst") (class "bind-rr"))
                                 (option "-")
                                 ,@(map (lambda (i)
                                          `(option (@ (value ,i)
                                                      ,@(awhen (prop event 'RRULE)
                                                               (awhen (rrule:wkst it)
                                                                      (awhen (eqv? it i)
                                                                             '((selected))))))
                                                   ,(week-day-name i)))
                                        (iota 7))))

                             ((byday)
                              (let ((input (lambda* (optional: (byday '(#f . #f)) key: final?)
                                             `(div (@ (class "unit" ,(if final? " final" "")))
                                                   ;; TODO make this thiner, and clearer that
                                                   ;; it belongs to the following dropdown
                                                   (input (@ (type number)
                                                             (value ,(awhen (car byday) it))))
                                                   (select (option "-")
                                                     ,@(map (lambda (i)
                                                              `(option (@ (value ,i)
                                                                          ,@(if (eqv? i (cdr byday))
                                                                                '((selected)) '()))
                                                                       ,(week-day-name i)))
                                                            (iota 7)))))))
                                ;; TODO how does this bind?
                                `(div (@ (class "bind-rr input-list"))
                                      ,@(cond ((and=> (prop event 'RRULE)
                                                      rrule:byday)
                                               => (lambda (it) (map input it)))
                                              (else '()))

                                      ,(input final?: #t))))

                             ((bysecond byminute byhour
                                        bymonthday byyearday
                                        byweekno bymonth bysetpos)
                              (let ((input
                                     (lambda* (value optional: (final ""))
                                       `(input (@ (class "unit " ,final)
                                                  (type "number")
                                                  (size 2)
                                                  (value ,value)
                                                  (min ,(case key
                                                          ((bysecond byminute byhour)  0)
                                                          ((bymonthday)              -31)
                                                          ((byyearday)              -366)
                                                          ((byweekno)                -53)
                                                          ((bymonth)                 -12)
                                                          ((bysetpos)               -366)
                                                          ))
                                                  (max ,(case key
                                                          ((bysecond)    60)
                                                          ((byminute)    59)
                                                          ((byhour)      23)
                                                          ((bymonthday)  31)
                                                          ((byyearday)  366)
                                                          ((byweekno)    53)
                                                          ((bymonth)     12)
                                                          ((bysetpos)   366))))))))
                                `(div (@ (name ,key)
                                         (class "bind-rr input-list"))
                                      ,@(map input
                                             (awhen (prop event 'RRULE)
                                                    (or ((case key
                                                           ((bysecond)   rrule:bysecond)
                                                           ((byminute)   rrule:byminute)
                                                           ((byhour)     rrule:byhour)
                                                           ((bymonthday) rrule:bymonthday)
                                                           ((byyearday)  rrule:byyearday)
                                                           ((byweekno)   rrule:byweekno)
                                                           ((bymonth)    rrule:bymonth)
                                                           ((bysetpos)   rrule:bysetpos))
                                                         it)
                                                        '())))
                                      ,(input '() "final"))))
                             (else (error "Unknown field, " key))))

                         ;; TODO enable this button
                         (td (button (@ (class "clear-input") (title "Rensa input")) "X"))
                         ))
                  '(freq until count interval bysecond byminute byhour
                         byday bymonthday byyearday byweekno bymonth bysetpos
                         wkst)
                  ; (prop event 'RRULE)
                  ))))


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
                           onclick: "place_in_edit_mode(event_from_popup(this.closest('.popup-container')))")
                      (btn "🗑"
                           title: "Ta bort"
                           onclick: "remove_event(event_from_popup(this.closest('.popup-container')))"))))

             ,(tabset
                `(("📅" title: "Översikt"
                   ,(fmt-single-event ev))

                  ,@(when (edit-mode)
                     `(("📅" title: "Redigera"
                        ,(fmt-for-edit ev))))

                  ("⤓" title: "Nedladdning"
                   (div (@ (class "eventtext") (style "font-family:sans"))
                        (h2 "Ladda ner")
                        (div (@ (class "side-by-side"))
                             (ul (li (a (@ (href "/calendar/" ,(prop ev 'UID) ".ics"))
                                        "som iCal"))
                                 (li (a (@ (href "/calendar/" ,(prop ev 'UID) ".xcs"))
                                        "som xCal")))
                             ,@(when (debug)
                                 `((ul
                                    (li (button (@ (onclick "console.log(event_to_jcal(event_from_popup(this.closest('.popup-container'))));")) "js"))
                                    (li (button (@ (onclick "console.log(jcal_to_xcal(event_to_jcal(event_from_popup(this.closest('.popup-container')))));")) "xml"))))))
                        ))

                  ,@(when (prop ev 'RRULE)
                      `(("↺" title: "Upprepningar" class: "repeating"
                         ,(editable-repeat-info ev)))))))))

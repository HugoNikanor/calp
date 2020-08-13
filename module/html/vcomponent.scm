(define-module (html vcomponent)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (datetime)
  :use-module (html util)
  :use-module ((html config) :select (edit-mode))
  :use-module ((html components) :select (btn tabset))
  :use-module ((output general) :select (calculate-fg-color))
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
            (case-lambda [(start) `(div (span (@ (class "dtstart")
                                                 (data-fmt "%L%H:%M"))
                                              ,start))]
                         [(start end) `(div (span (@ (class "dtstart")
                                                     ;; TODO same format string
                                                     ;; as fmt-time-span used
                                                     (data-fmt "%L%H:%M"))
                                                  ,start)
                                            " — "
                                            (span (@ (class "dtend")
                                                     (data-fmt "%L%H:%M"))
                                                  ,end))]))
         ,(when (and=> (prop ev 'LOCATION) (negate string-null?))
            `(div (b "Plats: ")
                  (div (@ (class "location"))
                       ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                    (prop ev 'LOCATION)))))
         ,(awhen (prop ev 'DESCRIPTION)
                 `(span (@ (class "description"))
                        ,(format-description ev it)))
         ,(awhen (prop ev 'RRULE)
                 `(span (@ (class "rrule"))
                        ,@(format-recurrence-rule ev)))
         ,(when (prop ev 'LAST-MODIFIED)
            `(span (@ (class "last-modified")) "Senast ändrad "
                   ,(datetime->string (prop ev 'LAST-MODIFIED) "~1 ~H:~M")))

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
                        `(a (@ (href "#" ,(date-link (as-date (prop ev 'DTSTART))))
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
                         " tentative"))
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
                 ,((@ (output xcal) ns-wrap)
                   ((@ (output xcal) vcomponent->sxcal)
                    ev)))))))


(define (repeat-info event)
  `(div (@ (class "eventtext"))
        (h2 "Upprepningar")
        (pre ,(prop event 'RRULE))))


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

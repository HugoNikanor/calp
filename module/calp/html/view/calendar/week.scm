(define-module (calp html view calendar week)
  :use-module (calp util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (datetime)
  :use-module (calp html view calendar shared)
  :use-module (calp html config)
  :use-module (calp html util)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (long-event?
                        event-length/day
                        event-zero-length?
                        events-between))
  :use-module ((calp html vcomponent)
               :select (make-block) )
  :use-module ((calp html components)
               :select (btn tabset ; form with-label
                            ))
  :use-module ((vcomponent group)
               :select (group-stream get-groups-between))
  )


(define*-public (render-calendar key: calendars events start-date end-date #:allow-other-keys)
  (let* ((long-events short-events (partition long-event? (stream->list (events-between start-date end-date events))))
         (range (date-range start-date end-date)))
    `((script "const VIEW='week';")
      (div (@ (class "calendar"))
           (div (@ (class "days"))
                ;; Top left area
                (div (@ (class "week-indicator"))
                     (span (@ (style "font-size: 50%")) "v.")  ; figure out if we want this...
                     ,@(->> (week-number start-date)
                            number->string string->list
                            (map (lambda (c) `(span ,(string c))))))
                ,@(time-marker-div)
                (div (@ (class "longevents event-container")
                        (data-start ,(date->string start-date) )
                        (data-end ,(date->string (add-day end-date)) )
                        (style "grid-column-end: span " ,(days-in-interval start-date end-date)))
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
                                        start-date end-date)))

                ,@(for event in (stream->list
                                 (events-between start-date end-date events))
                       `(popup-element
                         ;; TODO
                         (@ (class "vevent")
                            (data-uid ,(prop event 'UID)))
                         )
                       #;
                       ((@ (calp html vcomponent ) popup) ;
                       event (string-append "popup" (html-id event))))

                ))

      ;; description in sidebar / tab of popup
      (template (@ (id "vevent-description"))
                ,(description-template)
                )

      ;; edit tab of popup
      (template (@ (id "vevent-edit"))
                ,((@ (calp html vcomponent) edit-template)
                  calendars
                  ))

      ;; "physical" block
      (template (@ (id "vevent-block"))
                ,(block-template)
                )

      ;; Based on popup:s output
      (template
       (@ (id "popup-template"))
       (div (@ ; (id ,id)
             (class "popup-container CAL_"
               #; 
               ,(html-attr (or (prop (parent ev) 'NAME) ;
               "unknown")))
             (onclick "event.stopPropagation()"))
            (div (@ (class "popup"))
                 (nav (@ (class "popup-control"))
                      ,(btn "×"
                            title: "Stäng"
                            onclick: ""
                            ;; onclick: "close_popup(document.getElementById(this.closest('.popup-container').id))"
                            class: '("close-tooltip")))

                 (div (@ (class "tabgroup"))
                      (tab-element
                       (@ (title "Översikt"))
                       (span (@ (slot "label")) "📅")
                       (vevent-description
                        (@ (slot "content")
                           (class "vevent populate-with-uid"))))
                      (tab-element
                       (@ (title "Redigera"))
                       (span (@ (slot "label")) "🖊")
                       (vevent-edit (@ (slot "content")
                                       (class "populate-with-uid")))
                       )
                      ,@(when (debug)
                          `((tab-element
                             (@ (title "Debug"))
                             (span (@ (slot "label")) "🐸")
                             (vevent-dl (@ (slot "content")
                                           (class "populate-with-uid")))

                             ))))

                 ;; ,(tabset
                 ;;   `(("📅" title: "Översikt"
                 ;;      (vevent-description
                 ;;       (@ (class "vevent populate-with-uid")))
                 ;;      )

                 ;;     ,@(when (edit-mode)
                 ;;         `(("📅" title: "Redigera"
                 ;;            (vevent-edit (@ (class "populate-with-uid"))))))

                 ;;     ))
                 )))

      (template
       (@ (id "tab-template"))
       ;; ,((@ (calp html components) include-css) "/static/tab.css")
       (div (@ (class "tab"))
            (input (@ (type "radio")
                      ;; id
                      ;; (name ,tabgroup)
                      ))
            (label (@ ; for id
                    ;; style= top: calc(var(--tab-size) * i)
                    (title ; title
                     ))
                   (slot (@ (name "label")) "??")
                   )
            (div (@ (class "content"))
                 (slot (@ (name "content"))
                       (span (@ (class "error"))
                             "CONTENT MISSING")))))
      )))

;; based on the output of fmt-single-event
(define (description-template)
  '(div (@ (class " vevent eventtext summary-tab " ()))
        (h3 ((span (@ (class "repeating")) ; "↺"
                   )
             (span (@ (class "bind summary")
                      (data-property "summary")))))
        (div (div (time (@ (class "bind dtstart")
                           (data-property "dtstart")
                           (data-fmt "~L~H:~M")
                           (datetime ; "2021-09-29T19:56:46"
                            ))
                        ; "19:56"
                        )
                  "\xa0—\xa0"
                  (time (@ (class "bind dtend")
                           (data-property "dtend")
                           (data-fmt "~L~H:~M")
                           (datetime ; "2021-09-29T19:56:46"
                            ))
                        ; "20:56"
                        ))
             (div (@ (class "fields"))
                  (div (b "Plats: ")
                       (div (@ (class "bind location")
                               (data-property "location"))
                            ; "Alsättersgatan 13"
                            ))
                  (div (@ (class "bind description")
                          (data-property "description"))
                       ; "With a description"
                       )
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
                       )))))

(define (block-template)
  `(div (@ ; (id ,(html-id ev))
           (data-calendar "unknown")
           (class " CAL_unknown"
             ;; ,(when (and (prop ev 'PARTSTAT)
             ;;             (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
             ;;    " tentative")
             ;; ,(when (and (prop ev 'TRANSP)
             ;;             (eq? 'TRANSPARENT (prop ev 'TRANSP)))
             ;;    " transparent")
             )
           ; (onclick "toggle_popup('popup' + this.id)")
           )
        ;; Inner div to prevent overflow. Previously "overflow: none"
        ;; was set on the surounding div, but the popup /needs/ to
        ;; overflow (for the tabs?).
        (div (@ (class "event-body"))
             (span (@ (class "repeating")) ; "↺"
                    )
             (span (@ (class "bind summary")
                      (data-property "summary"))
                   ; ,(format-summary  ev (prop ev 'SUMMARY))
                   )
             (span (@ (class "bind location")
                       (data-property "location")))
             ;; Document symbol when we have text
             (span (@ (class "description"))
                    ; "🗎"
                    ))
        ) )


(define (time-marker-div)
  `((div (@ (class "sideclock"))
         ,@(map (lambda (time)
                  `(div (@ (class "clock clock-" ,time))
                        (span (@ (class "clocktext"))
                              ,time ":00")))
                (iota 12 0 2)))))

;; Lay out complete day (graphical)
;; (date . (events)) -> sxml
(define (lay-out-day day)
  (let* (((day-date . events) day)
         (time-obj (datetime date: day-date))
         (short-events (stream->list events))
         #;
         (zero-length-events short-events
                             (partition event-zero-length? (stream->list events))))

    (fix-event-widths!
     short-events
     event-length-key: (lambda (e)
                         (if (event-zero-length? e)
                             (time hour: 1)
                             (event-length/day day-date e))))

    `(div (@ (class "events event-container") (id ,(date-link day-date))
             (data-start ,(date->string day-date))
             (data-end ,(date->string (add-day day-date)) ))
          ,@(map (lambda (time)
                   `(div (@ (class "clock clock-" ,time))))
                 (iota 12 0 2))
          #;
          (div (@ (class "zero-width-events"))
               ,(map make-block zero-length-events))
          ,@(map (lambda (e) (create-block day-date e)) short-events))))



;; Format single event for graphical display
;; This is extremely simmilar to create-top-block, which currently recides in ./shared
(define (create-block date ev)
  ;; (define time (date->time-utc day))

  (define left  (* 100 (x-pos ev)))
  (define width* (* 100 (width ev)))
  (define top (if (date= date (as-date (prop ev 'DTSTART)))
                  (* 100/24
                     (time->decimal-hour
                      (as-time (prop ev 'DTSTART))))
                  0))
  (define height (* 100/24 (time->decimal-hour (event-length/day date ev))))


  (define style
    ;; The calc's here is to enable an "edit-mode".
    ;; Setting --editmode ≈ 0.8 gives some whitespace to the right
    ;; of the events, alowing draging there for creating new events.
    (if (edit-mode)
        (format #f "left:calc(var(--editmode)*~,3f%);width:calc(var(--editmode)*~,3f%);top:~,3f%;height:~,3f%;"

                left width* top height)
        (format #f "left:~,3f%;width:~,3f%;top:~,3f%;height:~,3f%;"
                left width* top height)))

  (make-block
   ev `((class
          ,(when (event-zero-length? ev)
             " zero-length")
          ,(when (date<? (as-date (prop ev 'DTSTART)) date)
             " continued")
          ,(when (and (prop ev 'DTEND) (date<? date (as-date (prop ev 'DTEND))))
             " continuing"))
        (style ,style))))

(define-module (calp html view calendar month)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (calp html view calendar shared)
  :use-module (calp html config)
  :use-module (vcomponent)
  :use-module (ice-9 match)
  :use-module ((vcomponent datetime)
               :select (really-long-event?
                        events-between))
  :use-module ((calp html vcomponent)
               :select (make-block output-uid))
  :use-module ((vcomponent util group)
               :select (group-stream get-groups-between))
  :export (render-calendar-table)
  )

;; (stream event-group) -> sxml
(define* (render-calendar-table key: events
                                start-date end-date
                                pre-start post-end
                                allow-other-keys:)

  (define-values (long-events short-events)
    ;; TODO should be really-long-event? or event-spanning-midnight
    (partition really-long-event? (stream->list (events-between pre-start post-end events))))

  (define short-event-groups
    (get-groups-between (group-stream (list->stream short-events))
                        pre-start post-end))

  (define long-event-groups
    (map (lambda (s)
           (define e (date+ s (date day: 6)))
           (cons* s e
                  (stream->list
                   (events-between s e (list->stream long-events)))))
         (date-range pre-start post-end (date day: 7))))

  `((script ,(lambda () (format #t "window.VIEW='month';")))
    (header (@ (class "table-head"))
            ,(string-titlecase (date->string start-date "~B ~Y")))
    (div (@ (class "caltable")
            (style "grid-template-rows: 2em"
              ,(string-concatenate
                (map (lambda (long-group)
                       (format #f " [time] 15pt [long] ~amm [short] 1fr"
                               (min 10 (* 4 (length (cddr long-group))))))
                     long-event-groups))))
         ,@(map (lambda (d) `(div (@ (class "thead")) ,(string-titlecase (week-day-name d))))
                (weekday-list))
         ,@(map (match-lambda*
                  (((s e events ...) i)
                   `(div (@ (class "cal-cell longevents event-container")
                            (style "grid-area: long " ,i ";"
                                   "grid-column: 1 / span 7;")
                            (data-start ,(date->string s))
                            (data-end ,(date->string (date+ e (date day: 1)))))
                         ,@(lay-out-long-events
                            s e events))))
                long-event-groups
                (iota (length long-event-groups) 1))

         ,@(caltable-time-cells start-date end-date
                                pre-start post-end)

         ,@(stream->list
            (stream-map
             (lambda (group i)
               (define day-date (car group))
               (define events (cdr group))
               `(div (@ (style "grid-area:short " ,i)
                        (class "cal-cell cal-cell-short event-container")
                        (data-start ,(date->string day-date))
                        (data-end ,(date->string (date+ day-date (date day: 1)))))
                     (div (@ (style "overflow-y:auto;"))
                      ,@(map make-small-block (stream->list events)))))
             short-event-groups
             (repeating-naturals 1 7)
             )))

    ;; These popups are relative the document root.
    ;; Can thus be placed anywhere in the DOM.
    ,@(for event in (stream->list
                     (events-between pre-start post-end events))
           `(popup-element
             (@ (class "vevent")
                (data-uid ,(output-uid event)))))

    (template
     (@ (id "vevent-block"))
     ;; TODO this is more or less copied verbatim from week's
     ;; version, warts and all. Figure out what should and shouldn't
     ;; be shared between the two.
     (div (@ (data-calendar "unknown"))
           (div (@ (class "event-body"))
                (span (@ (class "repeating")))
                (span (@ (class "summary")
                         (data-property "summary")))
                (span (@ (class "location")
                         (data-property "location"))))))
    ))



;;; Table output

(define (make-small-block event)
  (make-block event))

(define (caltable-time-cells start-date end-date
                             pre-start post-end)
  (map (lambda (day-date i)
         `(div (@ (style "grid-area:time " ,i)
                  (class "cal-cell cal-cell-time"))
               (a (@ (class "hidelink")
                     (href "/week/" ,(date->string day-date "~Y-~m-~d")
                           ".html#" ,(date->string day-date "~Y-~m-~d")))
                (time (@ (class "date-info "
                           ,(if (or (date< day-date start-date)
                                    (date< end-date day-date))
                                "non-current"
                                "current"))
                         (datetime ,(date->string day-date "~1")))
                      (span (@ (class "day-number"))
                            ,(date->string day-date "~e"))
                      ,(when (= 1 (day day-date))
                         `(span (@ (class "month-name"))
                                ,(date->string day-date "~b")))
                      ,(when (= 1 (month day-date) (day day-date))
                         `(span (@ (class "year-number"))
                                ", " ,(date->string day-date "~Y")))))))
       (date-range pre-start post-end)
       (map floor (iota (length (date-range pre-start post-end)) 1 1/7))))

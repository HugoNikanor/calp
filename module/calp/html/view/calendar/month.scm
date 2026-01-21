(define-module (calp html view calendar month)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (calp html view calendar shared)
  :use-module ((calp html util) :select (html-file-extension))
  :use-module (ice-9 match)
  :use-module (vcomponent)
  :use-module ((vcomponent data-stores common) :select (calendar-data-store?))
  :use-module ((vcomponent datetime)
               :select (instance-length instance-overlaps?))
  :use-module ((calp html vcomponent)
               :select (make-block))
  :use-module ((vcomponent data-stores query)
               :select (entries-between))
  :export (render-calendar-table)
  )

;; (stream event-group) -> sxml
(define* (render-calendar-table key: stores
                                start-date end-date
                                pre-start post-end
                                allow-other-keys:)
  (typecheck stores (list-of (pair-of string? calendar-data-store?)))
  (typecheck start-date date?)
  (typecheck end-date date?)
  (typecheck pre-start date?)
  (typecheck post-end date?)

  (define entries
    (map (lambda (t) (modify t (ref 2) (compose car vcomponent-children)))
         (stream->list (apply entries-between start-date end-date stores))))


  (define-values (long-events short-events)
    (partition (match-lambda ((_ _ ev) (datetime< (datetime day: 1) (instance-length ev))))
               entries))

  (define long-event-groups
    (map (lambda (week-start)
           (define e (date+ week-start (date day: 6)))
           (cons* week-start e
                  (filter (match-lambda ((_ _ ev) (instance-overlaps? ev week-start (date+ e (date day: 1)))))
                          long-events)))
         (date-range pre-start post-end 7)))

  (typecheck long-event-groups (list-of (pair-of* date? date? (list-of (tuple-of string? string? vevent?)))))
  ;; The grid-template-rows below depends on this being true
  (typecheck (length long-event-groups) (= (/ (days-in-interval pre-start post-end) 7)))

  `((script ,(lambda () (format #t "window.VIEW='month';")))
    (header (@ (class "table-head"))
            ,(string-titlecase (date->string start-date "~B ~Y")))
    (div (@ (class "caltable")
            ;; 2em for weekday names
            ;; Then for each "line" we have
            ;; - [time] 15pt (date number)
            ;; - [long] xxx (reserved space for long events)
            ;; - [short] 1fr (remaining space for events contained in day
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
            (stream-map (lambda (start week-offset)
                          (define end (date+ start (date day: 1)))
                          `(div (@ (style "grid-area:short " ,week-offset)
                                   (class "cal-cell cal-cell-short event-container")
                                   (data-start ,(date->string start))
                                   (data-end ,(date->string end)))
                                (div (@ (style "overflow-y:auto;"))
                                     ,@(map make-small-block
                                            (filter (match-lambda ((_ _ ev) (instance-overlaps? ev start end)))
                                                    short-events)))))
                        (stream-take (days-in-interval pre-start post-end)
                                     (date-stream (date day: 1) pre-start))
                        (repeating-naturals 1 7))))

    ;; TODO This is a very stupid set to create the popup-elements
    ;; which would be needed once javascript kicks in. REMOVE once
    ;; javascript part is rewritten.
    ,@(for _ in entries
           `(popup-element
             (@ (class "vevent")
                (data-uid "TODO" ; ,(output-uid event)
                          ))))

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

;;; pair tuple of calendar-id, href, vevent instance
(define (make-small-block tuple)
  (typecheck tuple (tuple-of string? string? vevent?))
  (apply make-block tuple))

(define (caltable-time-cells start-date end-date
                             pre-start post-end)
  (map (lambda (day-date i)
         `(div (@ (style "grid-area:time " ,i)
                  (class "cal-cell cal-cell-time"))
               (a (@ (class "hidelink")
                     (href "/week/" ,(date->string day-date "~Y-~m-~d")
                           "." ,(html-file-extension) "#" ,(date->string day-date "~Y-~m-~d")))
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

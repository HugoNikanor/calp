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

;;     februari 2026
;; må ti on to fr lö sö
;;                    1
;;  2  3  4  5  6  7  8
;;  9 10 11 12 13 14 15
;; 16 17 18 19 20 21 22
;; 23 24 25 26 27 28

;; Main body is a CSS grid with 7 columns, one for each week day.
;; The first row is the weekday names, then for each week 3 CSS rows
;; are used to create one "dispalyed" row. These are
;; - the date indicator
;; - the space for multi-day events
;; - the area for in-day events

;; (stream event-group) -> sxml
(define* (render-calendar-table key: stores
                                start-date
                                ;; end-date
                                ;; pre-start
                                ;; post-end
                                target-timezone
                                allow-other-keys:)
  (typecheck stores (list-of (pair-of string? calendar-data-store?)))
  (typecheck start-date date?)
  ;; (typecheck end-date date?)
  ;; (typecheck pre-start date?)
  ;; (typecheck post-end date?)
  (typecheck target-timezone string?)

  (define month-start (start-of-month start-date))
  (define month-end (end-of-month start-date))
  (define cal-start (start-of-week month-start))
  (define cal-end (end-of-week month-end))

  (define start-dt (datetime date: cal-start tz: target-timezone))
  (define end-dt (datetime date: (date+ cal-end (date day: 1)) tz: target-timezone))

  (define entries
    (map (lambda (t) (modify t (ref 2) (compose car vcomponent-children)))
         (stream->list (apply entries-between target-timezone start-dt end-dt stores))))


  (define-values (long-events short-events)
    (partition (match-lambda ((_ _ ev) (datetime< (datetime day: 1) (instance-length ev))))
               entries))

  (define long-event-groups
    (map (lambda (week-start)
           (define s (datetime date: week-start tz: target-timezone))
           (define e (datetime date: (date+ week-start (date day: 8)) tz: target-timezone))
           (list week-start (date+ week-start (date day: 7))
                 (filter (match-lambda ((_ _ ev) (instance-overlaps? target-timezone ev s e)))
                         long-events)))
         (date-range cal-start cal-end 7)))

  ;; (typecheck long-event-groups d
  ;;            (list-of (pair-of* date? date?
  ;;                               (list-of (tuple-of string? string? vevent?)))))

  ;; The grid-template-rows below depends on this being true

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
                               (min 10 (* 4 (length (list-ref long-group 2))))))
                     long-event-groups))))
         ,@(map (lambda (d) `(div (@ (class "thead")) ,(string-titlecase (week-day-name d))))
                (weekday-list))

         ,@(caltable-time-cells start-date)

         ,@(map (lambda (week group)
                  (define-values (group-start group-end group-members)
                    (apply values group))
                  `(div (@ (class "cal-cell longevents event-container")
                           (style ,(format #f "grid-area: long ~a;" week)
                             "grid-column: 1 / span 7;"))
                        ,@(lay-out-long-events
                           target-timezone group-start group-end
                           group-members)))
             ;; 10 is a number larger than the amount of weeks
             (iota 10 1)
             long-event-groups)

         ,@(map (lambda (week day)
                  `(div (@ (style ,(format #f "grid-area: short ~a" (1+ week)))
                           (class "cal-cell cal-cell-short event-container")
                           ;; data-start, data-end
                           )
                        (div (@ (style "overflow-y: auto"))
                             ,@(map make-small-block
                                    (filter (match-lambda
                                              ((_ _ ev)
                                               (instance-overlaps?
                                                target-timezone ev
                                                (datetime date: day tz: target-timezone)
                                                (datetime date: (date+ day (date day: 1))
                                                          tz: target-timezone))))
                                            short-events)))))
                (map (lambda (x) (floor-quotient x 7))
                     ;; 50 is a number larger than the amount of days in any month
                     (iota 50))
                (date-range cal-start cal-end)))


    


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

;; Generate grid cells containing date indicators
(define (caltable-time-cells target-month)
  (define start-date (start-of-month target-month))
  (define end-date   (end-of-month target-month))
  (define pre-start  (start-of-week start-date))
  (define post-end   (end-of-week end-date))
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

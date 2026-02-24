(define-module (calp html view calendar week)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (calp html view calendar shared)
  :use-module (calp html util)
  :use-module (vcomponent)
  :use-module ((vcomponent data-stores common) :select (calendar-data-store?))
  :use-module ((vcomponent datetime)
               :select (instance-overlaps?
                        instance-length
                        instance-length/clamped
                        instance-start-datetime
                        ))
  :use-module ((calp html vcomponent)
               :select (make-block) )
  :use-module (calp translation)
  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module ((vcomponent data-stores query)
               :select (entries-between))
  :export (render-calendar)
  )


;;; TODO much of this code conflates two different definitions of "a day"
;;; 1. A day is a specific date, such as 2026-03-11
;;; 2. A date is the time between one midnight and the next, in a given timezone,
;;;    for example 2026-03-11T00:00+01:00 - 2026-03-12T00:00+01:00
;;;
;;;
;;; In general, each graphical block works on the first definition of a day, while
;;; the set of relevant events depends on the second definition.
;;; When rendering the calendar, it will be given a date (1), and a timezone. From there
;;; it will create date (2) by doing `(datetime date: date-of-type-1 time: #00:00 tz: given-tz)`

(define-syntax-rule (with-object-on-backtrace object expr ...)
  (catch #t (lambda () expr ...)
    (lambda args
      (format (current-error-port) "object: ~s~%" object)
      (apply throw args))))

(define* (render-calendar key: stores start-date end-date (target-timezone "UTC")
                          allow-other-keys:)
  (typecheck stores (list-of (pair-of string? calendar-data-store?)))
  (typecheck start-date date?)
  (typecheck end-date   date?)
  (typecheck target-timezone string?)

  (define start-dt (datetime date: start-date tz: target-timezone))
  (define end-dt (datetime date: (date+ end-date (date day: 1))
                           tz: target-timezone))

  ;; Entries is a list of tuples, each containing:
  ;; - store identifier
  ;; - entry href
  ;; - entry instance
  (define entries
    (map (lambda (t) (modify t (ref 2) (compose car vcomponent-children)))
         (stream->list
          (apply entries-between target-timezone start-dt end-dt
                 stores))))

  (define-values (long-events short-events)
    (partition (match-lambda ((_ _ ev) (or (date? (prop1 ev 'DTSTART))
                                           (datetime< (datetime day: 1)
                                                      (instance-length ev)))))
               entries))

  (let* ((range (date-range start-date end-date)))
    `((script ,(lambda () (format #t "window.VIEW='week';")))
      (div (@ (class "calendar"))
           (div (@ (class "days"))
                ;; Top left area
                (div (@ (class "week-indicator"))
                     (span (@ (style "font-size: 50%"))
                           ,(G_ "v."))
                     ;; Split week-number into a span for each decimal,
                     ;; This allows vertial layouts
                     ,@(->>
                        ;; TODO un-tz
                        (week-number start-date)
                        number->string string->list
                        (map (lambda (c) `(span ,(string c))))))
                ,@(time-marker-div)
                (div (@ (class "longevents event-container")
                        (data-start ,(date->string start-date) )
                        (data-end ,(date->string end-date))
                        (style "grid-column-end: span "
                          ,(days-in-interval start-date end-date)))
                     ,@(lay-out-long-events target-timezone start-date end-date long-events))
                ,@(map (lambda (day-date)
                         `(div (@ (class "meta"))
                               (span (@ (class "daydate"))
                                     ,(date->string day-date (G_ "~Y-~m-~d")))
                               (span (@ (class "dayname"))
                                     ;; TODO translation here?
                                     ,(string-titlecase (date->string day-date "~a")))))
                       range)

                ,@(map (lambda (day) (lay-out-day target-timezone day short-events))
                       (date-range start-date end-date))

                ;; This creates the popup elements later "grabbed" by the JavaScript.
                ;; TODO remove this, and tell JavaScript to create it from templates
                ,@(for _ in entries
                       `(popup-element
                         (@ (class "vevent")
                            (data-uid "TODO" ; ,(output-uid event)
                                      ))))))


      ;; This template is here, instead of in (calp html calendar) since it only
      ;; applies to this specific view. (calp html calendar month) is assumed to
      ;; have its own variant of it.
      (template (@ (id "vevent-block"))
                ,(block-template))


      )))


;; "physical" block
(define (block-template)
  `(div (@ ; (id ,(html-id ev))
           (data-calendar "unknown")
           #;
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
             (span (@ (class "summary")
                      (data-property "summary"))
                   ; ,(format-summary  ev (prop ev 'SUMMARY))
                   )
             (span (@ (class "location")
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

(define (lay-out-day reference-zone day events)
  (typecheck day date?)
  (typecheck reference-zone string?)
  (typecheck events (list-of (tuple-of string? string? vevent?)))

  (define dt-start (datetime date: day tz: reference-zone))
  (define dt-end (datetime date: (date+ day (date day: 1))
                           tz: reference-zone))
  ;; - Find all instances overlapping [dt-start, dt-end)
  (define relevant-instances
    (filter (match-lambda ((_ _ ev)
                           (instance-overlaps? reference-zone ev dt-start dt-end)))
            events))

  (format (current-error-port)
          "Relevant instances for ~a: ~s~%" day (map (compose (extract1 'SUMMARY) caddr)
                                                     relevant-instances))

  ;; - Run fix-event-widths! on set of instances
  (fix-event-widths!
   reference-zone
   (map caddr events)
   event-length-key: (lambda (e) (instance-length/clamped dt-start dt-end reference-zone e)))

  ;; - plop them into the HTML container
  `(div (@ (class "events event-container")
           (id ,(date-link day))
           (data-start ,(date->string day))
           (data-end ,(date->string (date+ day (date day: 1)))))
        ,@(map (lambda (time) `(div (@ (class "clock clock-" ,time))))
               (iota 12 0 2))
        #;
        (div (@ (class "zero-width-events")) ; ; ; ; ;
        ,(map make-block zero-length-events))
        ,@(map (lambda (entry) (create-block day reference-zone entry))
               relevant-instances)))


;; Format single event for graphical display
;; This is extremely similar to create-top-block, which currently recides in ./shared
;; Before running this, `fix-event-widths!` must be called on the set
;; of instances which will reside in the same "day" block.
;; TODO fix naming conventions for all these *-block methods.
;; We can't have make-block AND create-block
(define (create-block day reference-zone entry)
  (typecheck day date?)
  (typecheck reference-zone string?)
  ;; (calendar-identifier href event)
  (typecheck entry (tuple-of string? string? vevent?))

  (define ev (list-ref entry 2))

  (define event-continued?
    (not
     (datetime</zoneinfo (datetime date: day tz: reference-zone)
                         (instance-start-datetime reference-zone ev))))

  (define left  (* 100 (x-pos ev)))
  (define width* (* 100 (width ev)))
  (define top
    (if event-continued?
        0
        (* 100/24
           (duration->decimal-hour
            (datetime-difference/zoneinfo
             (instance-start-datetime reference-zone ev)
             (datetime date: day tz: reference-zone))))))

  (define height (* 100/24 (duration->decimal-hour
                            (instance-length/clamped
                             (datetime date: day tz: reference-zone)
                             (datetime date: (date+ day (date day: 1))
                                       tz: reference-zone)
                             reference-zone
                             ev))))


  (define style
    ;; The calc's here is to enable an "edit-mode".
    ;; Setting --editmode ≈ 0.8 gives some whitespace to the right
    ;; of the events, alowing draging there for creating new events.
    (if ((@ (calp html config) edit-mode))
        (format #f "left:calc(var(--editmode)*~,3f%);width:calc(var(--editmode)*~,3f%);top:~,3f%;height:~,3f%;"

                left width* top height)
        (format #f "left:~,3f%;width:~,3f%;top:~,3f%;height:~,3f%;"
                left width* top height)))

  (make-block
   (list-ref entry 0)
   (list-ref entry 1)
   (list-ref entry 2)
   `((class
       ,(when (datetime= (datetime)
                         (instance-length ev))
          " zero-length")
       ,(when event-continued? " continued")
       ,(when (datetime</zoneinfo
               (datetime date: (date+ day (date day: 1)) tz: reference-zone)
               (datetime+ (instance-start-datetime reference-zone ev)
                          (instance-length ev)))
          " continuing"))
     (style ,style))))

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
                        instance-length/day
                        instance-zero-length?
                        instance-length))
  :use-module ((calp html vcomponent)
               :select (make-block) )
  :use-module (calp translation)
  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module ((vcomponent data-stores query)
               :select (entries-between))
  :export (render-calendar)
  )

(define-syntax-rule (with-object-on-backtrace object expr ...)
  (catch #t (lambda () expr ...)
    (lambda args
      (format (current-error-port) "object: ~s~%" object)
      (apply throw args))))

(define* (render-calendar key: stores start-date end-date allow-other-keys:)
  (typecheck stores (list-of (pair-of string? calendar-data-store?)))
  (typecheck start-date date?)
  (typecheck end-date date?)

  (define entries
    (map (lambda (t) (modify t (ref 2) (compose car vcomponent-children)))
         (stream->list (apply entries-between start-date end-date stores))))

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
                     ,@(->> (week-number start-date)
                            number->string string->list
                            (map (lambda (c) `(span ,(string c))))))
                ,@(time-marker-div)
                (div (@ (class "longevents event-container")
                        (data-start ,(date->string start-date) )
                        (data-end ,(date->string (date+ end-date (date day: 1))) )
                        (style "grid-column-end: span " ,(days-in-interval start-date end-date)))
                     ,@(lay-out-long-events start-date end-date long-events))
                ,@(map (lambda (day-date)
                         `(div (@ (class "meta"))
                               (span (@ (class "daydate"))
                                     ,(date->string day-date (G_ "~Y-~m-~d")))
                               (span (@ (class "dayname"))
                                     ;; TODO translation here?
                                     ,(string-titlecase (date->string day-date "~a")))))
                       range)
                ,@(lay-out-days short-events start-date end-date)

                ;; TODO This is a very stupid set to create the
                ;; popup-elements which would be needed once
                ;; javascript kicks in. REMOVE once javascript part is
                ;; rewritten.
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

(define (lay-out-days events start end)
  (typecheck events (list-of (tuple-of string? string? vevent?)))
  (typecheck start date?)
  (typecheck end date?)

  ;; NOTE This is supposed to only run on one day at a time, but apparently
  ;; it works just as well with multiple days. Might be a time bit slower
  (fix-event-widths!
   (map caddr events)
   event-length-key: (lambda (e)
                       (if (instance-zero-length? e)
                           (time hour: 1)
                           (instance-length/day start e))))


  ;; For each day, generate
  (map (lambda (start)
         (define end (date+ start (date day: 1)))
        `(div (@ (class "events event-container")
                 (id ,(date-link start))
                 (data-start ,(date->string start))
                 (data-end ,(date->string end)))
              ,@(map (lambda (time) `(div (@ (class "clock clock-" ,time))))
                     (iota 12 0 2))
              #;
              (div (@ (class "zero-width-events")) ; ; ; ;
              ,(map make-block zero-length-events))
              ,@(map (lambda (e) (with-object-on-backtrace
                             e (create-block start e)))
                     (filter (match-lambda ((_ _  ev)
                                            (instance-overlaps? ev start end)))
                             events))))
       (stream->list (days-in-interval start end)
                     (day-stream start)))

  )


;; Format single event for graphical display
;; This is extremely simmilar to create-top-block, which currently recides in ./shared
;; TODO fix naming conventions for all these *-block methods.
;; We can't have make-block AND create-block
(define (create-block date entry)
  (typecheck date date?)
  (typecheck entry (tuple-of string? string? vevent?))

  (define ev (list-ref entry 2))

  (define left  (* 100 (x-pos ev)))
  (define width* (* 100 (width ev)))
  (define top (if (date= date (as-date (prop1 ev 'DTSTART)))
                  (* 100/24
                     (time->decimal-hour
                      (as-time (prop1 ev 'DTSTART))))
                  0))
  (define height (* 100/24 (time->decimal-hour (instance-length/day date ev))))


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
       ,(when (instance-zero-length? ev)
          " zero-length")
       ,(when (date<? (as-date (prop1 ev 'DTSTART)) date)
          " continued")
       ,(when (and (prop% ev 'DTEND) (date<? date (as-date (prop1 ev 'DTEND))))
          " continuing"))
     (style ,style))))

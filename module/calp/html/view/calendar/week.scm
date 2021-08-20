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
  :use-module ((vcomponent group)
               :select (group-stream get-groups-between))
  )


(define*-public (render-calendar key: events start-date end-date #:allow-other-keys)
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
                       ((@ (calp html vcomponent ) popup) event (string-append "popup" (html-id event))))

                )))))



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

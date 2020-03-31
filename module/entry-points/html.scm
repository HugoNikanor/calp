(define-module (entry-points html)
  :export (main)
  :use-module (output html)
  :use-module (util)
  :use-module (util time)
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (ice-9 getopt-long)

  :use-module (util config all)
  )


(define opt-spec
  `((from (value #t) (single-char #\F))
    (to (value #t) (single-char #\T))
    (file (value #t) (single-char #\f))
    (count (value #t))
    (style (value #t) (predicate ,(lambda (v) (memv (string->symbol v)
                                            '(wide week unchunked table)))))))

(define (main args)
  (define opts (getopt-long args opt-spec))
  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                    [else (date+ start (date month: 1)) ]))

  (define count (string->number (option-ref opts 'count "12")))

  (define style (string->symbol (option-ref opts 'style "wide")))

  (define-values (calendars events)
    (load-calendars
     calendar-files: (cond [(option-ref opts 'file #f) => list]
                           [else (calendar-files)]) ))


  (report-time! "Calendars loaded")

  (case style
    [(unchunked)
     (html-generate calendars events start end render-calendar)]
    [(wide)                             ; previously `chunked'
     (html-chunked-main count calendars events start (date month: 1))]
    [(week)
     ;; TODO The small calendar is always centered on months, it might
     ;; be a good idea to instead center it on the current week, meaning
     ;; that the active row is always in the center
     (html-chunked-main count calendars events
                        (previous-week-start start (week-start))
                        (date day: 7))]
    [(table)
     (html-table-main count calendars events start)]
    [else
     (error "Unknown html style: ~a" style)]))

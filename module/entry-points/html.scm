(define-module (entry-points html)
  :export (main)
  :use-module (output html)
  :use-module (util)
  :use-module (util time)
  :use-module (util config)
  :use-module (util options)
  ;; :use-module (vcomponent)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (ice-9 getopt-long)
  )


(define opt-spec
  `((from (value #t) (single-char #\F)
          (description "Start date of output.")
          )
    (count (value #t)
           (description (*TOP* "How many pages should be rendered."
                               "If --style=" (b "week") " and --from=" (b "2020-04-27")
                               " then --count=" (b 4) " would render the four pages "
                               "2020-04-27, 2020-05-04, 2020-05-11, and 2020-05-25. "
                               "Defaults to 12 to give a whole year when --style=" (b "month") "."
                               )))

    (style (value #t) (predicate ,(lambda (v) (memv (string->symbol v)
                                            '(wide week table))))
           (description (*TOP* "How the body of the HTML page should be layed out. "
                               (br) (b "week")
                               " gives a horizontally scrolling page with 7 elements, "
                               "where each has events graphically laid out hour by hour."
                               (br) (b "table")
                               " gives a month in overview as a table. Each block contains "
                               "the events for the given day, in order of start time. They are "
                               "however not graphically sized. "
                               (br) (b "wide")
                               " is the same as week, but gives a full month."))
           )

    (help (single-char #\h) (description "Print this help."))))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))
  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define count (string->number (option-ref opts 'count "12")))

  (define style (string->symbol (option-ref opts 'style "wide")))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return)
    )

  (case style
    [(wide)                             ; previously `chunked'
     (html-chunked-main count start (date month: 1))]
    [(week)
     ;; TODO The small calendar is always centered on months, it might
     ;; be a good idea to instead center it on the current week, meaning
     ;; that the active row is always in the center
     (html-chunked-main count
                        (start-of-week start (get-config 'week-start))
                        (date day: 7))]
    [(table)
     (html-table-main count start)]
    [else
     (error "Unknown html style: ~a" style)]))

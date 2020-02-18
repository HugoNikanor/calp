(define-module (entry-points ical)
  :export (main)
  :use-module (util)
  :use-module (output ical)
  :use-module ((vcomponent) :select (load-calendars*))
  :use-module ((parameters) :select (calendar-files))
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  :use-module (datetime util)
  )

(define opt-spec
  '((from (value #t) (single-char #\F))
    (to (value #t) (single-char #\T))
    (file (value #t) (single-char #\f))))

(define (main args)
  (define opts (getopt-long args opt-spec))

  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end   (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                      ;; [else (normalize-date* (set (month start) = (+ 1)))]
                      [(date+ start (date month: 1))]
                      ))

  ;; TODO this contains repeated events multiple times
  (define-values (calendars regular repeating)
    (load-calendars* calendar-files: (cond [(option-ref opts 'file #f) => list]
                                           [else (calendar-files)]) ))

  (ical-main calendars regular repeating start end)
 )

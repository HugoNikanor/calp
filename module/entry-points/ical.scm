(define-module (entry-points ical)
  :export (main)
  :use-module (util)
  :use-module (output ical)
  :use-module ((vcomponent) :select (load-calendars))
  :use-module ((parameters) :select (calendar-files))
  :use-module (ice-9 getopt-long)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-19 util)
  )

(define opt-spec
  '((from (value #t) (single-char #\F))
    (to (value #t) (single-char #\T))))

(define (main args)
  (define opts (getopt-long args opt-spec))

  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end   (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                      [else (normalize-date* (set (date-month start) = (+ 1)))]))

  (define-values (calendars events)
    (load-calendars
     calendar-files: (cond [(option-ref opts 'file #f) => list]
                           [else (calendar-files)]) ))

  (ical-main calendars events start end)
 )

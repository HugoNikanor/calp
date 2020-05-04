(define-module (entry-points ical)
  :export (main)
  :use-module (util)
  :use-module (util options)
  :use-module (output ical)
  :use-module ((vcomponent) :select (load-calendars*))
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  :use-module (datetime util)
  )

(define opt-spec
  '((from (value #t) (single-char #\F))
    (to (value #t) (single-char #\T)
        (description "Returns all elements between these two dates."))
    (help (single-char #\h)
          (description "Print this help."))))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end   (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                      ;; [else (normalize-date* (set (month start) = (+ 1)))]
                      [(date+ start (date month: 1))]
                      ))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (ical-main start end))

(define-module (calp entry-points ical)
  :export (main)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (vcomponent formats ical output)
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  :use-module (calp translation)
  :use-module (calp translation)
  )

(define opt-spec
  `((from (value #t) (single-char #\F))
    (to (value #t) (single-char #\T)
        (description ,(G_ "Returns all elements between these two dates.")))
    (help (single-char #\h)
          (description ,(G_ "Print this help.")))))

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

  (print-events-in-interval start end))

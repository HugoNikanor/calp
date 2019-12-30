(define-module (entry-points benchmark)
  :export (main)

  :use-module (ice-9 getopt-long)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (parameters)
  )


(define opt-spec
  '((file (value #t) (single-char #\f))))

(define (main args)
  (define opts (getopt-long args opt-spec))

  (load-calendars* calendar-files: (cond [(option-ref opts 'file #f) => list]
                                         [else (calendar-files)])))

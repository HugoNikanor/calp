(define-module (entry-points benchmark)
  :export (main)

  :use-module (ice-9 getopt-long)
  :use-module (util)
  :use-module (vcomponent)
  )


(define opt-spec
  '((file (value #t) (single-char #\f))))

(define (main args)
  (define opts (getopt-long args opt-spec))

  (cond [(option-ref opts 'file #f) => (compose load-calendars* list)]
        [else (load-calendars)]))

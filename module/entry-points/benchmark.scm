(define-module (entry-points benchmark)
  :export (main)

  :use-module (ice-9 getopt-long)
  :use-module (util)
  :use-module (util app)
  )


(define opt-spec
  '())

(define (main args)
  (define opts (getopt-long args opt-spec))

  (write (getf 'calendars app: (current-app)))
)

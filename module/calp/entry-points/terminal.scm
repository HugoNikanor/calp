(define-module (calp entry-points terminal)
  :export (main)
  :use-module (calp terminal)
  :use-module (vcomponent)
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  :use-module (vulgar)
  :use-module (hnh util options)
  :use-module (calp translation)
  )

(define options
  `((date (value #t) (single-char #\d)
          (description ,(_ "Which date to start on.")))
    (help (single-char #\t) (description ,(_ "Print this help.")))
    ))

(define (main args)
  (define opts (getopt-long args (getopt-opt options)))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (let ((date (or (and=> (option-ref opts 'date #f) parse-freeform-date)
                  (current-date))))
    (with-vulgar
     (lambda () (main-loop date))))
)

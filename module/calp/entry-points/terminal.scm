(define-module (calp entry-points terminal)
  :export (%summary main)
  :use-module (calp terminal)
  :use-module (vcomponent)
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  :use-module (vulgar)
  :use-module (hnh util options)
  :use-module (calp translation)
  )

(define-public %category 'application)

(define %summary
  (G_ "loads the calendars, and starts an interactive terminal interface."))

(define options
  `((date (value #t) (single-char #\d)
          (description ,(G_ "Which date to start on.")))
    (help (single-char #\h) (description ,(G_ "Print this help.")))
    ))

(define (main args)
  (define opts (getopt-long args (getopt-opt options)))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (let ((date (or (and=> (option-ref opts 'date #f) parse-freeform-datetime)
                  (current-date))))
    (with-vulgar
     (lambda () (main-loop date)))))

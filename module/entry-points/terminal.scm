(define-module (entry-points terminal)
  :export (main)
  :use-module (output terminal)
  :use-module (vcomponent)
  :use-module (ice-9 getopt-long)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-19 util)
  :use-module (parameters)
  :use-module (vulgar)
  )

(define options
  '((date (value #t) (single-char #\d))
    (file (value #t) (single-char #\f))))

(define (main args)
  (define opts (getopt-long args options))
  (define-values (calendars events)
    (load-calendars
     calendar-files: (cond [(option-ref opts 'file #f) => list]
                           [else (calendar-files)]) ))

  (let ((time (date->time-utc
               (drop-time (or (and=> (option-ref opts 'date #f) parse-freeform-date)
                              (current-date))))))
    ;; (format (current-error-port) "len(events) = ~a~%" (stream-length events))
    (with-vulgar
     (lambda () (main-loop time events))))
)
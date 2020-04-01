(define-module (entry-points terminal)
  :export (main)
  :use-module (output terminal)
  :use-module (vcomponent)
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (vulgar)
  )

(define options
  '((date (value #t) (single-char #\d))
    (file (value #t) (single-char #\f))))

(define (main args)
  (define opts (getopt-long args options))
  (define-values (calendars events)
    (cond [(option-ref opts 'file #f) => (compose load-calendars list)]
          [else (load-calendars)]))

  (let ((date (or (and=> (option-ref opts 'date #f) parse-freeform-date)
                  (current-date))))
    ;; (format (current-error-port) "len(events) = ~a~%" (stream-length events))
    (with-vulgar
     (lambda () (main-loop date events))))
)

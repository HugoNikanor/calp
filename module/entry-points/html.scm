(define-module (entry-points html)
  :export (main)
  :use-module (output html)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (srfi srfi-19 alt)
  :use-module (srfi srfi-19 alt util)
  :use-module (ice-9 getopt-long)

  :use-module (parameters)
  )


(define opt-spec
  '((from (value #t) (single-char #\F))
    (to (value #t) (single-char #\T))
    (file (value #t) (single-char #\f))
    (chunked)))

(define (main args)
  (define opts (getopt-long args opt-spec))
  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                    [else (date+ start (date month: 1)) ]))

  (define-values (calendars events)
    (load-calendars
     calendar-files: (cond [(option-ref opts 'file #f) => list]
                           [else (calendar-files)]) ))

  ((@ (srfi srfi-41) stream->list) events)

  (if (option-ref opts 'chunked #f)
      (html-chunked-main calendars events start)
      (html-generate calendars events start end)))

(define-module (entry-points benchmark)
  :export (main)

  :use-module (ice-9 getopt-long)
  :use-module (util options)
  :use-module (util)
  :use-module (srfi srfi-41)
  )


(define opt-spec
  `((field (value #t)
           (description
            "Which field from the current app to force. Most heavy fields are defined in "
            (i "(vcomponent)") ". Required."))
    (enable-output (single-char #\o)
                   (description
                    "Output is be default supressed, since many fields contain way to much data "
                    "to read. This turns it on again."))
    (help (single-char #\h) (description "Print this help."))))


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define field (and=> (option-ref opts 'field #f) string->symbol))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (unless field
   (throw 'argument-error "Field `field' required."))

  (let ((strm ((@ (vcomponent instance) get-event-set)
               (@ (vcomponent instance) global-event-object))))
    (if (option-ref opts 'enable-output #f)
        (write (stream->list 1000 strm))
        (stream->list 1000 strm))))

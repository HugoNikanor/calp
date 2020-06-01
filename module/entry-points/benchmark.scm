(define-module (entry-points benchmark)
  :export (main)

  :use-module (ice-9 getopt-long)
  :use-module (util options)
  :use-module (util)
  :use-module (util app)
  )


(define opt-spec
  `((field (value #t)
           (description
            (*TOP*
             "Which field from the current app to force. Most heavy fields are defined in "
             (i "(vcomponent)") ".")))
    (enable-output (single-char #\o)
                   (description
                    (*TOP*
                     "Output is be default supressed, since many fields contain way to much data "
                     "to read. This turns it on again.")))
    (help (single-char #\h) (description "Print this help."))))


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define field (and=> (option-ref opts 'field #f) string->symbol))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (unless field
   (throw 'argument-error "Field `field' required."))

  (aif (option-ref opts 'enable-output #f)
       (write (getf field app: (current-app)))
       (getf field app: (current-app))))

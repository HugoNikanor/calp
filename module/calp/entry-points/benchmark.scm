(define-module (calp entry-points benchmark)
  :use-module (util)

  :use-module (ice-9 getopt-long)
  :use-module (util options)
  :use-module ((srfi srfi-41) :select (stream->list))

  :use-module ((vcomponent instance methods) :select (get-event-set))
  :autoload (vcomponent instance) (global-event-object)

  :export (main)
  )


(define opt-spec
  `((enable-output (single-char #\o)
                   (description
                    "Output is be default supressed, since many fields contain way to much data "
                    "to read. This turns it on again."))
    (help (single-char #\h) (description "Print this help."))))


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))


  (awhen (option-ref opts '() #f)
         ((module-ref (resolve-module
                       `(calp benchmark ,@(map string->symbol it)))
                      'run-benchmark)))
)

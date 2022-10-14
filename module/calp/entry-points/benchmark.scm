(define-module (calp entry-points benchmark)
  :use-module (hnh util)

  :use-module (ice-9 getopt-long)
  :use-module (hnh util options)
  :use-module ((srfi srfi-41) :select (stream->list))

  :use-module (calp translation)

  :use-module ((vcomponent util instance methods) :select (get-event-set))
  :autoload (vcomponent util instance) (global-event-object)

  :export (main)
  )


(define opt-spec
  `((enable-output (single-char #\o)
                   (description
                    ,(G_ "Output is by default supressed, since many fields contain way to much data to read. This turns it on again.")
                    ))
    (help (single-char #\h) (description ,(G_ "Print this help.")))))


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))


  (let ((opt (option-ref opts '() #f)))
    (if (null? opt)
        (print-arg-help opt-spec)
        ((module-ref (resolve-module
                      `(calp benchmark ,@(map string->symbol opt)))
                     'run-benchmark)))))

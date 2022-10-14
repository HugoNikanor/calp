(define-module (calp entry-points convert)
  :export (main)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module ((hnh util path) :select (filename-extension))
  :use-module (ice-9 getopt-long)
  :use-module (sxml simple)
  :use-module (calp translation)
  )



(define opt-spec
  `((from (single-char #\f) (value (options "xcal" "ical"))
          (description ,(xml->sxml (G_ "<group>Input format (otherwise infered from <i>infile</i>)</group>"))))
    (to (single-char #\t) (value (options "xcal" "ical"))
        (description ,(xml->sxml (G_ "<group>Output format (otherwise infered from <i>outfile</i>)</group>"))))
    (infile (value #t) (single-char #\i) (description ,(G_ "Input file")))
    (outfile (value #t) (single-char #\o) (description ,(G_ "Output file")))
    (help (single-char #\h) (description ,(G_ "Print this help.")))))


(define (filename-to-type filename)
  (let ((extension (filename-extension filename)))
    (cond [(string-ci=? "ics" extension)
           "ical"]
          [(or (string-ci=? "xcal" extension)
               (string-ci=? "xml" extension))
           "xcal"])))


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define infile "/dev/stdin")
  (define outfile "/dev/stdout")
  (define from "ical")
  (define to "xcal")

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (awhen (option-ref opts 'infile #f)
         (set! infile it
               from (filename-to-type it)))

  (awhen (option-ref opts 'outfile #f)
         (set! outfile it
               to (filename-to-type it)))

  (awhen (option-ref opts 'from #f)
         (set! from it))

  (awhen (option-ref opts 'to #f)
         (set! to it))

  ;; from ∈ { "ical" "xcal" }
  ;;   to ∈ { "ical" "xcal" }

  (let ()
   (define parser
     (case (string->symbol from)
       [(ical)
        ;; read ical
        (@ (vcomponent formats ical parse) parse-calendar)]
       [(xcal)
        ;; read xcal
        (compose
         (@ (vcomponent formats xcal parse) sxcal->vcomponent)
         ;; TODO strip *TOP*
         xml->sxml)]
       [else (scm-error 'misc-error "convert-main"
                        (G_ "Unexpected parser type: ~a")
                        (list from) #f)]
       ))

   (define writer
     (case (string->symbol to)
       [(ical)
        ;; write ical
        (lambda (component port)
          (display ((@ (vcomponent formats ical output) component->ical-string)
                    component)
                   port))]
       [(xcal)
        ;; write xcal
        (lambda (component port)
          (sxml->xml ((@ (vcomponent formats xcal output) vcomponent->sxcal)
                      component)
                     port))]
       [else (scm-error 'misc-error "convert-main"
                        (G_ "Unexpected writer type: ~a")
                        (list to) #f)]))


   (call-with-output-file outfile
     (lambda (p)
       (writer (call-with-input-file infile parser)
               p)))
   (newline)))

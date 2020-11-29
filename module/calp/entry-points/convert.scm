(define-module (calp entry-points convert)
  :export (main)
  :use-module (calp util)
  :use-module (calp util options)
  :use-module (ice-9 getopt-long)
  :use-module (sxml simple)
  )



(define opt-spec
  `((from (single-char #\f) (value (options "xcal" "ical")) 
          (description "Input format (infered from " (i "infile") ")"))
    (to (single-char #\t) (value (options "xcal" "ical"))
        (description "Output format (infered from " (i "outfile") ")"))
    (infile (value #t) (single-char #\i) (description "Input file"))
    (outfile (value #t) (single-char #\o) (description "Output file"))
    (help (single-char #\h) (description "Print this help."))))


(define (filename-to-type filename)
  (let ((extension (car (reverse (string-split filename #\.)))))
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
        (@ (vcomponent ical parse) parse-calendar)]
       [(xcal)
        ;; read xcal
        (compose
         (@ (vcomponent xcal parse) sxcal->vcomponent)
         ;; TODO strip *TOP*
         xml->sxml)]
       [else (error "")]
       ))

   (define writer
     (case (string->symbol to)
       [(ical)
        ;; write ical
        (lambda (component port)
          (display ((@ (vcomponent ical output) component->ical-string)
                    component)
                   port))]
       [(xcal)
        ;; write xcal
        (lambda (component port)
          (sxml->xml ((@ (vcomponent xcal output) vcomponent->sxcal)
                      component)
                     port))]
       [else (error "")]))


   (call-with-output-file outfile
     (lambda (p)
       (writer (call-with-input-file infile parser)
               p)))
   (newline)))

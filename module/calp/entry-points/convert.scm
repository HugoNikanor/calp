(define-module (calp entry-points convert)
  :use-module (vcomponent media-type)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 regex)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (calp translation)
  :use-module (sxml simple)
  :use-module ((text markup) :select (sxml->ansi-text))
  :export (main %summary))

(define %summary
  (G_ "Convert between calendar media types."))

(define-public %category 'static)

(define options
  `((input (single-char #\i)
           (value #t)
           (description ,(G_ "Input file")))
    (output (single-char #\o)
            (value #t)
            (description ,(G_ "Output file")))
    (from (single-char #\f)
          (value #t)
          (description
           ,(G_ "Media type of input")))
    (to (single-char #\t)
        (value #t)
        (description
         ,(G_ "Media type of output")))
    (help (single-char #\h)
          (description ,(G_ "Print this help")))))

(define (module-help)
  (string-append
   "<group>"
   (G_ "<p>Usage: <b>convert</b> [<i>flags</i> ...]</p>")
   (G_ "<p>By default, media type is infered from the input and output filename</p>")
   "</group>"))

(define (print-help)
  (-> (module-help)
      xml->sxml sxml->ansi-text
      display))

(define (main args)
  (define opts (getopt-long args (getopt-opt options)))

  (when (option-ref opts 'help #f)
    (print-help)
    (print-arg-help options)
    (throw 'return))

  (define input (option-ref opts 'input "-"))
  (define output (option-ref opts 'output "-"))
  (define intype (option-ref opts 'from "text/calendar"))
  (define outtype (option-ref opts 'to "text/calendar"))

  (define input-format (resolve-media-type intype))

  (define output-format (resolve-media-type outtype))

  ;; TODO handle - as substitude for stdin and stdout
  (let ((component
         (call-with-input-file input
           (parser input-format))))
    (call-with-output-file output
      (lambda (port)
       ((serializer output-format)
        component port
        ;; TODO many serializers have options to prettify output
        ;; Figure out a way to conditionally pass those here
        )))))

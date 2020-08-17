(define-module (calp entry-points text)
  :export (main)
  :use-module (text flow)
  :use-module (ice-9 getopt-long)
  :use-module (util io)
  :use-module (util options)
  )


(define options
  '((width (value #t) (single-char #\w)
           (description "Width of written text, defaults to 70 chars."))
    (file (value #t) (single-char #\f)
          (description "Read from " (i "file") " instead of standard input."))
    (help (single-char #\h)
          (description "Prints this help."))))

(define (main args)
  (define opts (getopt-long args (getopt-opt options)))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (for-each (lambda (l) (display l) (newline))
            (flow-text
             (with-input-from-port (open-input-port (option-ref opts 'file "-"))
               (@ (ice-9 rdelim) read-string))
             #:width (or (string->number (option-ref opts 'width "")) 70))))

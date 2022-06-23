(define-module (calp entry-points text)
  :export (main)
  :use-module (text flow)
  :use-module (ice-9 getopt-long)
  :use-module (hnh util io)
  :use-module (hnh util options)
  :use-module (calp translation)
  :use-module (sxml simple)
  :use-module (srfi srfi-88)
  )


(define options
  `((width (value #t) (single-char #\w)
           (description ,(_ "Width of written text, defaults to 70 chars.")))
    (file (value #t) (single-char #\f)
          (description ,(xml->sxml (_ "<group>Read from <i>file</i> instead of standard input.</group>"))))
    (help (single-char #\h)
          (description ,(_ "Prints this help.")))))

(define (main args)
  (define opts (getopt-long args (getopt-opt options)))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (for-each (lambda (l) (display l) (newline))
            (flow-text
             (with-input-from-port (let ((fname (option-ref opts 'file "-")))
                                     (if (string=? fname "-")
                                         (current-input-port)
                                         (open-input-file fname)))
               (@ (ice-9 rdelim) read-string))
             width: (or (string->number (option-ref opts 'width "")) 70))))

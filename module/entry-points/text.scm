(define-module (entry-points text)
  :export (main)
  :use-module (output text)
  :use-module (ice-9 getopt-long)
  :use-module (util io)
  )


(define options
  '((width (value #t) (single-char #\w))
    (file (value #t) (single-char #\f))
    ))

(define (main opts)
 (for-each (lambda (l) (display l) (newline))
           (flow-text
            (with-input-from-port (open-input-port (option-ref opts 'file "-"))
              (@ (ice-9 rdelim) read-string))
            #:width (or (string->number (option-ref opts 'width "")) 70))))

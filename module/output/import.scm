(define-module (output import)
  :use-module (util))

(define options
  '((calendar (value #t) (single-char #\c))
    (source (value #t) (single-char #\f))
    ))

(define (import-main calenadrs events args)
  (define opts (getopt-long args options))

  (define calendar (option-ref opts 'calendar #f))

  (unless calendar
    (format (current-error-port)
            "Everything wroong~%"))


  ;; TODO save sourcetype and dir for vdir calendars

  #;
  (let ((component (make-vcomponent (option-ref args 'source "/dev/stdin")))) ;
                                        ;
    ;; Check UID                        ;
    ;; Add to calendar                  ;
    ;; Allocate file, save there        ;
                                        ;
  )


  )

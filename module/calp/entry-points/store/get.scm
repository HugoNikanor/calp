(define-module (calp entry-points store get)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 match)
  :use-module (hnh util)
  :use-module (hnh util io)
  :use-module (hnh util options)
  :use-module (vcomponent)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type)
  :export (main %summary))

(define-public %summary
  (G_ "Retrieve entries by href."))

(define opt-spec
  `((media (value media-type)
           (description ,(G_ "Media type to format output as, such as text/calendar")))))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)
                            stop-at-first-non-option: #t))
  (define media-module
    (map string->symbol (string-split (option-ref opts 'media "text/calendar") #\/)))

  (define media-type
    (module-ref (resolve-interface `(vcomponent media-type ,@media-module))
                'format))

  (for href in (option-ref opts '() '())
       ;; TODO make header configurable
       ;; (format #t "== ~a ==~%" href)
       (match (string-split href #\/)
         ((store-name local-href)
          (cond
           ((assoc-ref
             ((@ (vcomponent config) data-stores))
             store-name)
            => (lambda (store)
                 (cond ((get-by-href store local-href)
                        => (lambda (ev)
                             ((serializer media-type)
                              ev (current-output-port))
                             (ensure-newline)))
                       (else
                        (format (current-error-port) "No such entry: ~a~%"
                                local-href)
                        ;; TODO Error code
                        (throw 'return)))))
           (else
            (format (current-error-port) "No such store: ~a~%"
                    store-name)
            ;; TODO error code
            (throw 'return))))
         (other (format (current-error-port)
                        "Error: All references MUST be on form \"<store>/<href>\". Got ~s~%"
                        other)
                ;; TODO error code
                (throw 'return)))))

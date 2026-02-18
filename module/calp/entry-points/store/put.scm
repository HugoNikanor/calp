(define-module (calp entry-points store put)
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


(define %summary
  (G_ "Add entries to store"))

(define opt-spec
  `((force (single-char #\f)
           (description ,(G_ "Force insert even if entry already exists.")))
    (media (value #t)
           (description ,(G_ "Media type of the input"))))

  )


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)
                            stop-at-first-non-option: #t))

  (define media-module
    (map string->symbol (string-split (option-ref opts 'media "text/calendar") #\/)))

  (define media-type
    (module-ref (resolve-interface `(vcomponent media-type ,@media-module))
                'format))

  (define-values (href content)
    (match (option-ref opts '() '())
      ((or (href)
           (href "-"))
       (values href ((parser media-type) (current-input-port))))
      ((href file)
       (values href (call-with-input-file file (parser media-type))))
      (()
       ;; TODO better error
       (scm-error 'misc-error "calp store put"
                  "Bad command line usage"
                  '() #f))))

  (match (string-split href #\/)
    ((store-name local-href)
     (cond ((assoc-ref ((@ (vcomponent config) data-stores))
                       store-name)
            => (lambda (store)
                 (if (or (option-ref opts 'force #f)
                         (not (get-by-href store local-href)))
                     (begin
                       (put-event! store local-href content)
                       (flush! store))
                     (format (current-error-port)
                             "Refused to put entry, already exists. Try --force~%"))))

           ;; TODO proper error (no such store)
           (else
            (format (current-error-port) "No such store: ~a~%"
                    store-name)
            (throw 'return))))
    (_ (throw 'a-proper-error))))

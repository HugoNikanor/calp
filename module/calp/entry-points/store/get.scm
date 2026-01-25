(define-module (calp entry-points store get)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 getopt-long)
  :use-module (hnh util)
  :use-module (hnh util io)
  :use-module (hnh util options)
  :use-module (vcomponent)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type)
  :export (main %summary))

(define-public %summary
  (G_ "Retrieve entry by href."))


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

  ;; NOTE this retrieves a single entry, and prints all
  ;; the recurrence instances for it. If the entry isn't
  ;; found, or isn't recurring, then the behaviour is
  ;; undefined.
  (for-each
   (lambda (href)
     ;; TODO make header configurable
     (format #t "== ~a ==~%" href)
     ;; TODO handle hrefs without '/'
     (let ((store-name local-href (apply values (string-split href #\/)))) ;
       (cond
        ((assoc-ref                             ;
          ((@ (vcomponent config) data-stores)) ;
          store-name)
         => (lambda (store)
              (cond ((get-by-href store local-href)
                     => (lambda (ev)
                          ;; TODO make base instance or recursion set configurable
                          ;; TODO allow max date for recursion set
                          ((serializer media-type)
                           ev (current-output-port))
                          (ensure-newline)
                          #;
                          (stream-for-each ; ; ;
                          (lambda (instance) ; ; ;
                          (format #t "~a - ~a: ~a~%" ; ; ;
                          (prop1 instance 'DTSTART) ; ; ;
                          (prop1 instance 'DTEND) ; ; ;
                          (prop1 instance 'SUMMARY))) ; ; ;
                          (generate-recurrence-set ev))))
                    ;; TODO proper error (no such entry)
                    (else
                     (format (current-error-port) "No such entry~%")
                     (throw 'return)))))
        ;; TODO proper error (no such store)
        (else
         (format (current-error-port) "No such store~%")
         (throw 'return)))))
   (option-ref opts '() '()))
 
  )

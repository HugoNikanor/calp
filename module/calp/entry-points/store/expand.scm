(define-module (calp entry-points store expand)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (ice-9 getopt-long)
  :use-module (hnh util)
  :use-module (hnh util io)
  :use-module (hnh util options)
  :use-module (hnh test util)
  :use-module (datetime)
  :use-module (vcomponent)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type)
  :export (main %summary))

(define %summary "Expands recursion set for given entries.")

(define opt-spec
  `((from (single-char #\f)
          (value #t)
          (description ,(G_ "Lower bound for recursion set to show.")))
    (to (single-char #\t)
        (value #t)
        (description ,(G_ "Upper bound for recursion set to show.")))))


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)
                            stop-at-first-non-option: #t))

  (define start
    (datetime
     date: (cond ((option-ref opts 'start #f) => date->string)
                 (else (date- (current-date) (date day: 7))))
     tz: "UTC"))
  (define end
    (datetime
     date: (cond ((option-ref opts 'end #f) => date->string)
                 (else (date+ (current-date) (date day: 7))))
     tz: "UTC"))

  (define outer-t1 (transform-time-of-day (gettimeofday)))

  ;; NOTE this retrieves a single entry, and prints all
  ;; the recurrence instances for it. If the entry isn't
  ;; found, or isn't recurring, then the behaviour is
  ;; undefined.
  (for href in (option-ref opts '() '())
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

                            ;; TODO start timing here
                            (define t1 (transform-time-of-day (gettimeofday)))

                            (->> (generate-recurrence-set ev)
                                 (stream-take-while ;
                                  (lambda (instance) (datetime< (ensure-zoned-datetime "UTC" (prop1 instance 'DTSTART)) ;
                                                           end)))

                                 (stream-filter ; ;
                                  (lambda (instance)
                                    ;; (format (current-error-port) "[~a, ~a), [~a, ~a)~%"
                                    ;;         start end (ensure-zoned-datetime "UTC" (prop1 instance 'DTSTART)) ; ;
                                    ;;         (datetime+ (ensure-zoned-datetime "UTC" (prop1 instance 'DTSTART)) ; ;
                                    ;;                    (datetime day: 1)))
                                    (timespan-overlaps? ; ;
                                     start end          ; ;
                                     (ensure-zoned-datetime "UTC" (prop1 instance 'DTSTART)) ; ;
                                     (datetime+ (ensure-zoned-datetime "UTC" (prop1 instance 'DTSTART)) ; ;
                                                (datetime day: 1)))))

                                 (stream-for-each
                                  (lambda (instance)
                                    (format #t "~a - ~a: ~a~%" ; ; ;
                                            (prop1 instance 'DTSTART) ; ; ;
                                            (prop1 instance 'DTEND) ; ; ;
                                            (prop1 instance 'SUMMARY))) ; ; ;
                                  ))

                            ;; TODO end timing here
                            (define t2 (transform-time-of-day (gettimeofday)))

                            (format #t "Δt₁ = ~as~%" (/ (- t2 t1) (µs 1.0)))
                            ))
                      ;; TODO proper error (no such entry)
                      (else
                       (format (current-error-port) "No such entry: ~a~%"
                               local-href)
                       (throw 'return)))))
          ;; TODO proper error (no such store)
          (else
           (format (current-error-port) "No such store: ~a~%"
                   store-name)
           (throw 'return))))
       )

  (define outer-t2 (transform-time-of-day (gettimeofday)))

  (format #t "outer Δt₁ = ~as~%" (/ (- outer-t2 outer-t1) (µs 1.0)))
  )

;;; Commentary:
;;; List entries in a given set of calendar stores
;;; Code:
(define-module (calp entry-points list)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (hnh util options)
  :use-module (calp translation)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util color)
  :use-module (vcomponent)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent data-stores common)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (web uri)
  :use-module (datetime)
  :use-module (hnh test util)
  :use-module ((vcomponent data-stores query)
               :select (entries-between))
  :export (main %summary))

(define %summary "List entries in a given set of calendar stores")

(define opt-spec
  `((help (single-char #\h)
          (description ,(G_ "Print this help.")))
    (href (single-char #\h)
          (value href)
          (description ,(G_ "Explicit entry to list")))
    (tz (value #t)
        (description ,(G_ "Timezone to resolve local datetimes in.")))))

(define (format-time-interval start end)
  (typecheck start (or date? datetime?))
  (typecheck end   (or false? duration? date? datetime?))
  ;; Trailing spaces in some cases to allow them to line up
  (cond ((and (date? start) (date? end) (date= start end))
         (date->string start))

        ((and (date? start) (date? end))
         (format #f "~a - ~a "
                 (date->string start)
                 (date->string end)))

        ((and (datetime? start)
              (datetime? end)
              (date= (datetime-date start)
                     (datetime-date end)))
         (format #f "~a ~a-~a  "
                 (datetime->string start "~Y-~m-~d")
                 (datetime->string start "~H:~M")
                 (datetime->string end "~H:~M")))

        (else
         (define start-str
           (if (datetime? start)
               (datetime->string start "~Y-~m-~d ~H:~M")
               (date->string     start "~Y-~m-~d")))
         (cond ((duration? end)
                (format #f "~a, ~a,"
                        start-str
                        ;; TODO better duration formatting
                        (duration->string end)))
               (else start-str)))))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define zone
    (or (option-ref opts 'tz #f)
        (getenv "TZ")
        ((@ (datetime localtime) get-localtime))))

  (format #t "== ~a ==~%"
          (G_ "Configured Stores"))
  (for (href . store) in ((@ (vcomponent config) data-stores))
       (display "- ")
       (awhen (store-color store)
              (let ((r g b _ (->rgb/values it)))
                (format #t "\x1b[48;2;~a;~a;~am  \x1b[m "
                        r g b)))

       (format #t "~a~%" (store-displayname store))
       (format #t "  href: ~a~%" href)
       (format #t "  ~@?~%" (G_ "~a entries") (entry-count store))
       (awhen (store-description store)
              ;; TODO flow text
              (format #t "    ~a~%" it))
       ;; TODO? store-calendar-timezone
       (format #t "  ~a~%" (uri->string (store-uri store)))
       )


  (newline)

  (cond ((option-ref opts 'href #f)
         => (lambda (href)
              ;; NOTE this retrieves a single entry, and prints all
              ;; the recurrence instances for it. If the entry isn't
              ;; found, or isn't recurring, then the behaviour is
              ;; undefined.
              (format #t "== ~a ==~%" href)
              (let ((store-name local-href (apply values (string-split href #\/)))) ;
                (cond
                 ((assoc-ref                             ;
                   ((@ (vcomponent config) data-stores)) ;
                   store-name)
                  => (lambda (store)
                       (cond ((get-by-href store local-href)
                              => (lambda (ev)
                                   (stream-for-each
                                    (lambda (instance)
                                      (format #t "~a - ~a: ~a~%"
                                              (prop1 instance 'DTSTART)
                                              (prop1 instance 'DTEND)
                                              (prop1 instance 'SUMMARY)))
                                    (generate-recurrence-set ev)
                                    )))
                             ;; TODO proper error (no such entry)
                             (else (throw 'error)))))
                 ;; TODO proper error (no such store)
                 (else (throw 'error))))))

        (else
         (format #t "== ~a ==~%" (G_ "Entries in Interval"))
         (define-values (start end)
          (match (option-ref opts '() '())
            ((start end)
             (values (datetime date: (parse-iso-date start) tz: zone)
                     (datetime date: (parse-iso-date end)   tz: zone)))
            ((start) (let ((s (parse-iso-date start))
                           (n (current-date)))
                       (if (date< s n)
                           (values (datetime date: s tz: zone) (datetime date: n tz: zone))
                           (values (datetime date: n tz: zone) (datetime date: s tz: zone)))))
            (() (let ((n (current-date)))
                  (values (datetime date: n tz: zone)
                          (datetime date: (date+ n (date day: 1)) tz: zone))))))

         (format #t "~a - ~a~%"
                 (datetime->string start "~Y-~m-~d ~H:~M~z")
                 (datetime->string end   "~Y-~m-~d ~H:~M~z"))

         (newline)

         (define t1 (transform-time-of-day (gettimeofday)))

         (stream-for-each
          (print-entry ((@ (vcomponent config) data-stores)))
          (apply entries-between zone start end
                 ((@ (vcomponent config) data-stores))))

         (define t2 (transform-time-of-day (gettimeofday)))

         (format #t "Δt₁ = ~as~%" (/ (- t2 t1) (µs 1.0)))
         ))
  ;; - Get list of configured data stores
  ;; - For each data store, query entries in [start, end)
  ;;   + this must include recurring events correctly
  ;; - output the result, noting which entries are recurring
  '1)

(define (print-entry store-alist)
  (match-lambda
    ((store-prefix href entry)
     (define store (assoc-ref store-alist store-prefix))
     (awhen (store-color store)
            (let ((r g b _ (->rgb/values it)))
              (format #t "\x1b[38;2;~a;~a;~am"
                      r g b)))
     (define el (car (vcomponent-children entry)))
     (format #t "~a ~a \x1b[2m<~a/~a>"
             (format-time-interval
              (prop1 el 'DTSTART)
              (or (prop1 el 'DTEND)
                  (prop1 el 'DURATION)))
             (prop1 el 'SUMMARY)
             store-prefix
             href)
     (format #t "\x1b[m~%")
     )))


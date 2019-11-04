(define-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (vcomponent recurrence)
  #:use-module (vcomponent timezone)
  #:use-module (vcomponent base)
  #:use-module (vcomponent parse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-19 setters)
  #:use-module (srfi srfi-26)
  #:use-module (util)
  #:export (parse-calendar)
  #:re-export (repeating? make-vcomponent))

;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO attribute can be subtracted from
;; the event DTSTART to get UTC time.

(re-export-modules (vcomponent base))

(define string->time-utc
  (compose date->time-utc parse-datetime))

(define (parse-dates! cal)
  "Parse all start times into scheme date objects."

  #;
  (for tz in (filter (lambda (o) (eq? 'VTIMEZONE (type o))) (children cal))
       (for-each (lambda (p) (mod! (attr p "DTSTART") string->time-utc))
                 (children tz))

       ;; TZSET is the generated recurrence set of a timezone
       (set! (attr tz 'X-HNH-TZSET)
             (make-tz-set tz)))

  (for ev in (filter (lambda (o) (eq? 'VEVENT (type o))) (children cal))
       (let ((tz (getenv "TZ")))
         (aif (prop (attr* ev 'DTSTART) 'TZID)
              (setenv "TZ" (car it))
              (unsetenv "TZ"))
         (let*
             ((dptr (attr* ev 'DTSTART))
              (eptr (attr* ev 'DTEND))

              (date (parse-datetime (value dptr)))
              (end-date
               (cond ;; [(attr ev 'DURATION) => (lambda (d) (add-duration ...))]
                [(not eptr)
                 (let ((d (set (date-hour date) = (+ 1))))
                   (set! (attr ev 'DTEND) d
                         eptr (attr* ev 'DTEND))
                   d)]
                [(value eptr) => parse-datetime]
                [else
                 (set (date-hour date) = (+ 1))])))

           (set! (value dptr) (date->time-utc date)
                 (value eptr) (date->time-utc end-date))

           (when (prop (attr* ev 'DTSTART) 'TZID)
             ;; (format (current-error-port) "date = ~a~%" date)
             (set! (zone-offset date) (zone-offset (time-utc->date (value dptr))))
             ;; (format (current-error-port) "date = ~a~%" date)
                   ;; set! (zone-offset date) (get-tz-offset ev)

             (set!
                 (value dptr) (date->time-utc date)

                 ;; The standard says that DTEND must have the same
                 ;; timezone as DTSTART. Here we trust that blindly.
                 (zone-offset end-date) (zone-offset date)
                 (value eptr) (date->time-utc end-date))))


         (setenv "TZ" tz))))


(define* (parse-calendar path)
  (let ((component (parse-cal-path path)))
    ;; let*
    #;
    ((component
      (case (string->symbol (or (attr root "X-HNH-SOURCETYPE") "no-type"))
        ;; == Single ICS file ==
        ;; Remove the abstract ROOT component,
        ;; returning the wanted VCALENDAR component
        ((file)
         ;; TODO test this when an empty file is given.
         (car (children root)))

        ;; == Assume vdir ==
        ;; Also removes the abstract ROOT component, but also
        ;; merges all VCALENDAR's children into the a newly
        ;; created VCALENDAR component, and return that component.
        ;;
        ;; TODO the other VCALENDAR components might not get thrown away,
        ;; this since I protect them from the GC in the C code.
        ((vdir)
         (let ((accum (make-vcomponent 'VCALENDAR))
               (ch (children root)))

           ;; Copy attributes from our parsed VCALENDAR
           ;; to our newly created one.
           (unless (null? ch)
             (for key in (attributes (car ch))
                  (set! (attr accum key) (attr (car ch) key))))

           ;; Merge all children
           (let ((tz '()))
             (for cal in ch
                  (for component in (children cal)
                       (case (type component)
                         ((VTIMEZONE)
                          ;; (set! tz (cons component tz))
                          (unless (find (lambda (o) (and (eq? 'VTIMEZONE (type o))
                                                    (string=? (attr o "TZID")
                                                              (attr component "TZID"))))
                                        (children accum))
                            (add-child! accum component)))
                         ((VEVENT)
                          (add-child! accum component)
                          )
                         (else => (lambda (type)
                                    (format (current-error-port)
                                            "Got unexpected component of type ~a~%" type))
                               #; (add-child! accum component)
                               ))))

             (unless (null? tz)
               (add-child! accum (car tz)))
             )
           ;; return
           accum))

        ((no-type) (error 'no-type)))))

    (parse-dates! component)

    (unless (attr component "NAME")
      (set! (attr component "NAME")
        (or (attr component "X-WR-CALNAME")
            "[NAMELESS]")))

    #;
    (unless (attr component "COLOR")
      (set! (attr component "COLOR")
        (attr root      "COLOR")))

    ;; return
    component))

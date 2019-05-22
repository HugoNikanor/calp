(define-module (vcomponent)
  #:use-module ((vcomponent primitive) :select (%vcomponent-make))
  #:use-module (vcomponent datetime)
  #:use-module (vcomponent recurrence)
  #:use-module (vcomponent timezone)
  #:use-module (vcomponent base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-19 setters)
  #:use-module (srfi srfi-26)
  #:use-module (util)
  #:export (make-vcomponent)
  #:re-export (repeating?))

;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO attribute can be subtracted from
;; the event DTSTART to get UTC time.

(re-export-modules (vcomponent base))

(define string->time-utc
  (compose date->time-utc parse-datetime))

(define (parse-dates! cal)
  "Parse all start times into scheme date objects."

  (for tz in (children cal 'VTIMEZONE)
       (for-each (lambda (p) (mod! (attr p "DTSTART") string->time-utc))
                 (children tz))

       ;; TZSET is the generated recurrence set of a timezone
       (set! (attr tz 'X-HNH-TZSET)
             (make-tz-set tz)
             #;
             ((@ (srfi srfi-41) stream)
              (list
               (car (children tz))
               (cadr (children tz))))
             ))

  (for ev in (children cal 'VEVENT)
       (define dptr (attr* ev 'DTSTART))
       (define eptr (attr* ev 'DTEND))

       (define date     (parse-datetime (value dptr)))
       (define end-date
         (if (value eptr)
             (parse-datetime (value eptr))
             (set (date-hour date) = (+ 1))))

       (set! (value dptr) (date->time-utc date)
             (value eptr) (date->time-utc end-date))

       (when (prop (attr* ev 'DTSTART) 'TZID)
         (set! (zone-offset date) (get-tz-offset ev)
               (value dptr) (date->time-utc date)

               ;; The standard says that DTEND must have the same
               ;; timezone as DTSTART. Here we trust that blindly.
               (zone-offset end-date) (zone-offset date)
               (value eptr) (date->time-utc end-date)))))


;; (define-public value caar)
;; (define-public next cdr)
;; (define-public next! pop!)


;; (define-public (reset! attr-list)
;;   (while (not (car attr-list))
;;     (next! attr-list))
;;   (next! attr-list))

;; value
;; (define-public v
;;   (make-procedure-with-setter car set-car!))



(define* (make-vcomponent #:optional path)
  (if (not path)
      (%vcomponent-make)
      (let* ((root (%vcomponent-make path))
             (component
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
                 (let ((accum (make-vcomponent))
                       (ch (children root)))
                   (set! (type accum) "VCALENDAR")

                   (unless (null? ch)
                    (for key in (attributes (car ch))
                         (set! (attr accum key) (attr (car ch) key))))

                   (for cal in ch
                        (for component in (children cal)
                             (case (type component)
                               ((VTIMEZONE)
                                (unless (find (lambda (z)
                                                (string=? (attr z "TZID")
                                                          (attr component "TZID")))
                                              (children accum 'VTIMEZONE))
                                  (push-child! accum component)))
                               (else (push-child! accum component)))))
                   ;; return
                   accum))

                ((no-type) (throw 'no-type))

                (else (throw 'something)))))

        (parse-dates! component)

        (unless (attr component "NAME")
          (set! (attr component "NAME")
                (or (attr component "X-WR-CALNAME")
                    (attr root      "NAME"))))

        (unless (attr component "COLOR")
          (set! (attr component "COLOR")
                (attr root      "COLOR")))

        ;; return
        component)))

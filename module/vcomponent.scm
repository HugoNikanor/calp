(define-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (vcomponent recurrence)
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

(define (parse-dates! cal)
  "Parse all start times into scheme date objects."

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
             ;; Re-align date to have correect timezone. This is since time->date gives
             ;; correct, but the code above may (?) fail to update the timezone.
             (set! (zone-offset date) (zone-offset (time-utc->date (value dptr)))
                   (value dptr) (date->time-utc date)

                   ;; The standard says that DTEND must have the same
                   ;; timezone as DTSTART. Here we trust that blindly.
                   (zone-offset end-date) (zone-offset date)
                   (value eptr) (date->time-utc end-date))))


         (setenv "TZ" tz))))


(define* (parse-calendar path)
  (let ((component (parse-cal-path path)))
    (parse-dates! component)

    (unless (attr component "NAME")
      (set! (attr component "NAME")
        (or (attr component "X-WR-CALNAME")
            "[NAMELESS]")))

    ;; return
    component))

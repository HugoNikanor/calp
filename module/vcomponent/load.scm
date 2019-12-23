(define-module (vcomponent load)
  :export (load-calendars)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (parameters)
  ;; :use-module (vcomponent)
  :use-module (vcomponent base)
  :use-module ((vcomponent parse) :select (parse-cal-path))
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?)))


;; Reads all calendar files from disk, and creates a list of "regular" events,
;; and a stream of "repeating" events, which are passed in that order to the
;; given procedure @var{proc}.
;;
;; Given as a sepparate function from main to ease debugging.
(define* (load-calendars #:key (calendar-files (calendar-files)))
  (define calendars (map parse-cal-path calendar-files))
  (define events (concatenate
                  ;; TODO does this drop events?
                  (map (lambda (cal) (filter (lambda (o) (eq? 'VEVENT (type o)))
                                        (children cal)))
                       calendars)))

  (let* ((repeating regular (partition repeating? events)))

    (set! repeating (sort*! repeating time<? (extract 'DTSTART))
          regular   (sort*! regular   time<? (extract 'DTSTART)))

    (values
     calendars
     (interleave-streams
      ev-time<?
      (cons (list->stream regular)
            (map generate-recurrence-set repeating))))))

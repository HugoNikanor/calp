(define-module (vcomponent load)
  :export (load-calendars load-calendars*)
  :use-module (util)
  :use-module (util time)
  :use-module (srfi srfi-1)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (parameters)
  ;; :use-module (vcomponent)
  :use-module (vcomponent base)
  :use-module ((vcomponent parse) :select (parse-cal-path))
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?)))


;; Reads all calendar files from disk, generate recurence-sets for all repeating events,
;; and returns a list of calendars, and a stream of all events "ready" for display.
(define* (load-calendars #:key (calendar-files (calendar-files)))
  (report-time! "Parsing ~a calendars" (length calendar-files))
  (let* ((calendars regular repeating (load-calendars* #:calendar-files calendar-files)))
    (report-time! "Calendars loaded, interleaving and reccurring")
    (values
     calendars
     (interleave-streams
      ev-time<?
      (cons (list->stream regular)
            (map generate-recurrence-set repeating)
            )))))

;; Basic version, loads calendrs, sorts the events, and returns
;; regular and repeating events separated from each other.
;; 
;; (list string) → (list calendar), (list event), (list event)
(define* (load-calendars* #:key (calendar-files (calendar-files)))

  (define calendars (map parse-cal-path calendar-files))
  (define events (concatenate
                  ;; TODO does this drop events?
                  (map (lambda (cal) (filter (lambda (o) (eq? 'VEVENT (type o)))
                                        (children cal)))
                       calendars)))

  (report-time! "Parse done, partitioning...")
  (let* ((repeating regular (partition repeating? events)))

    (report-time! "Sorting")
    ;; NOTE There might be instances where we don't care if the
    ;; collection if sorted, but for the time beieng it's much
    ;; easier to always sort it.
    (values calendars
            (sort*! regular   date/-time<? (extract 'DTSTART))
            (sort*! repeating date/-time<? (extract 'DTSTART)))))

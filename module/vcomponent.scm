(define-module (vcomponent)
  :use-module (util)
  :use-module (util app)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module (vcomponent load)
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?))
  :re-export (make-vcomponent
              parse-cal-path parse-calendar
              load-calendars load-calendars*))

(re-export-modules (vcomponent base))


(define-method (init-app calendar-files)
  (setf 'calendars (load-calendars calendar-files))

  (setf 'events
        (concatenate
         ;; TODO does this drop events?
         (map (lambda (cal) (filter (lambda (o) (eq? 'VEVENT (type o)))
                               (children cal)))
              (getf 'calendars))))

  (setf 'fixed-and-repeating-events
        (let* ((repeating regular (partition repeating? (getf 'events))))

          ;; (report-time! "Sorting")
          ;; NOTE There might be instances where we don't care if the
          ;; collection if sorted, but for the time beieng it's much
          ;; easier to always sort it.
          (list
           (sort*! regular   date/-time<? (extract 'DTSTART))
           (sort*! repeating date/-time<? (extract 'DTSTART)))))

  (setf 'fixed-events (car (getf 'fixed-and-repeating-events)))
  (setf 'repeating-events (cadr (getf 'fixed-and-repeating-events)))

  (setf 'event-set
        (interleave-streams
         ev-time<?
         (cons (list->stream (getf 'fixed-events))
               (map generate-recurrence-set (getf 'repeating-events)))))

  (setf 'uid-map
        (let ((ht (make-hash-table)))
          (for-each (lambda (event) (hash-set! ht (attr event 'UID) event)) (getf 'events))
          ht)))

(define-method (fixed-events-in-range start end)
  (filter-sorted (lambda (ev) ((in-date-range? start end)
                          (as-date (attr ev 'DTSTART))))
                 (getf 'fixed-events)))

(define-method (get-event-by-uid uid)
  (hash-ref (getf 'uid-map) uid))

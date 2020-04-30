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


;; TODO rename function
(define (calculate-recurrence-set regular repeating)
  (interleave-streams
   ev-time<?
   (cons (list->stream regular)
         (map (@ (vcomponent recurrence) generate-recurrence-set) repeating)
         )))


(define-method (init-app calendar-files)
  (setf app 'calendars (load-calendars calendar-files))

  (setf app 'events
        (concatenate
         ;; TODO does this drop events?
         (map (lambda (cal) (filter (lambda (o) (eq? 'VEVENT (type o)))
                               (children cal)))
              (getf app 'calendars))))

  (setf app 'fixed-and-repeating-events
        (let* ((repeating regular (partition repeating? (getf app 'events))))

          ;; (report-time! "Sorting")
          ;; NOTE There might be instances where we don't care if the
          ;; collection if sorted, but for the time beieng it's much
          ;; easier to always sort it.
          (list
           (sort*! regular   date/-time<? (extract 'DTSTART))
           (sort*! repeating date/-time<? (extract 'DTSTART)))))

  (setf app 'fixed-events (car (getf app 'fixed-and-repeating-events)))
  (setf app 'repeating-events (cadr (getf app 'fixed-and-repeating-events)))

  (setf app 'event-set (calculate-recurrence-set
                        (getf app 'fixed-events)
                        (getf app 'repeating-events)))

  (setf app 'uid-map
        (let ((ht (make-hash-table)))
          (for-each (lambda (event) (hash-set! ht (attr event 'UID) event)) (getf app 'events))
          ht)))

(define-method (fixed-events-in-range start end)
  (filter-sorted (lambda (ev) ((in-date-range? start end)
                          (as-date (attr ev 'DTSTART))))
                 (getf app 'fixed-events)))

(define-method (get-event-by-uid uid)
  (hash-ref (getf app 'uid-map) uid))

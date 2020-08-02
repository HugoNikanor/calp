(define-module (vcomponent instance methods)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (datetime)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?))
  :use-module (oop goops)

  :export (add-event
           remove-event

           get-event-by-uid
           fixed-events-in-range

           get-event-set get-calendars
           get-fixed-events get-repeating-events
           ))

(define-public (load-calendars calendar-files)
  (map parse-cal-path calendar-files))

;;; TODO both add- and remove-event sometimes crash with
;;;;; Warning: Unwind-only `stack-overflow' exception; skipping pre-unwind handler.
;;; I belive this is due to how getf and setf work.



;; == vcomponent ==
;; - calendar
;; - events
;; - repeating-events
;; - fixed-events
;; - event-set
;; - uid-map



(define-class <events> ()
  (calendar-files init-keyword: calendar-files:)
  (calendars getter: get-calendars)
  (events getter: get-events)
  (repeating-events getter: get-repeating-events)
  (fixed-events getter: get-fixed-events)
  (event-set getter: get-event-set)
  uid-map
  )


(define-method (get-event-by-uid (this <events>) uid)
  (hash-ref (slot-ref this 'uid-map) uid))



(define-method (fixed-events-in-range (this <events>) start end)
  (filter-sorted (lambda (ev) ((in-date-range? start end)
                          (as-date (prop ev 'DTSTART))))
                 (slot-ref this 'fixed-events)))


(define-method (initialize (this <events>) args)
  (next-method)

  (format (current-error-port) "Building <events> from ~a~%"
          (slot-ref this 'calendar-files))

  (slot-set! this 'calendars (load-calendars (slot-ref this 'calendar-files)))

  (slot-set! this 'events
             (concatenate
              (map (lambda (cal) (remove
                             (extract 'X-HNH-REMOVED)
                             (filter (lambda (o) (eq? 'VEVENT (type o)))
                                     (children cal))))
                   (slot-ref this 'calendars))))

  (let* ((repeating regular (partition repeating? (slot-ref this 'events))))
    (slot-set! this 'fixed-events     (sort*! regular   date/-time<? (extract 'DTSTART)))
    (slot-set! this 'repeating-events (sort*! repeating date/-time<? (extract 'DTSTART))))


  (slot-set! this 'event-set
             (interleave-streams
              ev-time<?
              (cons (list->stream (slot-ref this 'fixed-events))
                    (map generate-recurrence-set (slot-ref this 'repeating-events)))))

  (slot-set! this 'uid-map
             (let ((ht (make-hash-table)))
               (for-each (lambda (event) (hash-set! ht (prop event 'UID) event))
                         (slot-ref this 'events))
               ht)))

;;; TODO what should happen when an event with that UID already exists
;;; in the calendar? Fail? Overwrite? Currently it adds a second element
;;; with the same UID, which is BAD.
(define-method (add-event (this <events>) calendar event)

  (add-child! calendar event)
  (unless (prop event 'UID)
    (set! (prop event 'UID) (uuidgen)))




  (slot-set! this 'events
             (cons event (slot-ref this 'events)))

  (let* ((slot-name (if (repeating? event) 'repeating-events 'fixed-events))
         (events (slot-ref this slot-name)))
    (slot-set! this slot-name (insert-ordered event events ev-time<?)))

  (slot-set! this 'event-set
             (interleave-streams
              ev-time<?
              (list (if (repeating? event)
                        (generate-recurrence-set event)
                        (stream event))
                    (slot-ref this 'event-set))))

  (hash-set! (slot-ref this 'uid-map) (prop event 'UID)
             event)

  (prop event 'UID))




(define-method (remove-event (this <events>) event)
  (slot-set! this 'events (delete event (slot-ref this 'events)))

  (let ((slot-name (if (repeating? event) 'repeating-events 'fixed-events)))
    (slot-set! this slot-name
               (delete event (slot-ref this slot-name))))

  (slot-set! this 'event-set
             (stream-remove
              (lambda (ev)
                (equal? (prop ev 'UID)
                        (prop event 'UID)))
              (slot-ref this 'event-set)))

  (hash-set! (slot-ref this 'uid-map) (prop event 'UID)
             #f))


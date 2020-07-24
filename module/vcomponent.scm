(define-module (vcomponent)
  :use-module (util)
  :use-module (util app)
  :use-module (util config)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (datetime)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?))
  :re-export (make-vcomponent
              parse-cal-path parse-calendar))

(re-export-modules (vcomponent base))

(define-config calendar-files '()
  "Which files to parse. Takes a list of paths or a single string which will be globbed."
  pre: (lambda (v)
         (cond [(list? v) v]
               [(string? v) ((@ (glob) glob) v)]
               [else #f])))

(define-public (load-calendars calendar-files)
  (map parse-cal-path calendar-files))


(define-method (init-app calendar-files)
  (setf 'calendars (load-calendars calendar-files))

  (setf 'events
        (concatenate
         ;; TODO does this drop events?
         (map (lambda (cal) (remove
                        (extract 'X-HNH-REMOVED)
                        (filter (lambda (o) (eq? 'VEVENT (type o)))
                                (children cal))))
              (getf 'calendars))))

  (let* ((repeating regular (partition repeating? (getf 'events))))
    (setf 'fixed-events     (sort*! regular   date/-time<? (extract 'DTSTART)))
    (setf 'repeating-events (sort*! repeating date/-time<? (extract 'DTSTART))))


  (setf 'event-set
        (interleave-streams
         ev-time<?
         (cons (list->stream (getf 'fixed-events))
               (map generate-recurrence-set (getf 'repeating-events)))))

  (setf 'uid-map
        (let ((ht (make-hash-table)))
          (for-each (lambda (event) (hash-set! ht (prop event 'UID) event)) (getf 'events))
          ht)))

(define-method (fixed-events-in-range start end)
  (filter-sorted (lambda (ev) ((in-date-range? start end)
                          (as-date (prop ev 'DTSTART))))
                 (getf 'fixed-events)))

(define-method (get-event-by-uid uid)
  (hash-ref (getf 'uid-map) uid))




;;; TODO both add- and remove-event sometimes crash with
;;;;; Warning: Unwind-only `stack-overflow' exception; skipping pre-unwind handler.
;;; I belive this is due to how getf and setf work.


;;; TODO what should happen when an event with that UID already exists
;;; in the calendar? Fail? Overwrite? Currently it adds a second element
;;; with the same UID, which is BAD.
(define-public (add-event calendar event)

  (add-child! calendar event)

  (unless (prop event 'UID)
    (set! (prop event 'UID) (uuidgen)))

  (let ((events (getf 'events)))
    (setf 'events (cons event events)))

  (if (repeating? event)
      (let ((repeating (getf 'repeating-events)))
        (setf 'repeating-events (insert-ordered event repeating ev-time<?)))
      (let ((regular (getf 'fixed-events)))
        (setf 'fixed-events (insert-ordered event regular ev-time<?))))

  (let ((event-set (getf 'event-set)))
    (setf 'event-set
          (interleave-streams
           ev-time<?
           (list (if (repeating? event)
                     (generate-recurrence-set event)
                     (stream event))
                 event-set))))

  (hash-set! (getf 'uid-map) (prop event 'UID)
             event)

  (prop event 'UID))


(define-public (remove-event event)

  (let ((events (getf 'events)))
    (setf 'events (delete event events)))

  (if (repeating? event)
      (let ((repeating (getf 'repeating-events)))
        (setf 'repeating-events (delete event repeating)))
      (let ((regular (getf 'fixed-events)))
        (setf 'fixed-events (delete event regular))))

  (let ((event-set (getf 'event-set)))
    (setf 'event-set
          (stream-remove
           (lambda (ev)
             (equal? (prop ev 'UID)
                     (prop event 'UID)))
           event-set)))

  (hash-set! (getf 'uid-map) (prop event 'UID)
             #f))



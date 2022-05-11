(define-module (vcomponent util instance methods)
  :use-module (hnh util)
  :use-module (hnh util uuid)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (datetime)
  :use-module (vcomponent base)
  ;; :use-module (vcomponent parse)
  :use-module ((vcomponent util parse-cal-path) :select (parse-cal-path))
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?))
  :use-module (oop goops)

  :use-module (calp translation)

  :export (add-event
           remove-event

           get-event-by-uid
           fixed-events-in-range

           get-calendar-by-name

           get-event-set get-calendars
           get-fixed-events get-repeating-events

           add-and-save-event

           add-calendars
           ))

(define-public (load-calendars calendar-files)
  (map parse-cal-path calendar-files))


(define-class <events> ()
  ;; Files which calendars where loaded from
  (calendar-files init-keyword: calendar-files:
                  init-value: '())
  ;; calendar objects
  (calendars getter: get-calendars
             init-value: '())
  ;; events, which should all be children of the calendars
  (events getter: get-events)
  ;; subset of events
  (repeating-events getter: get-repeating-events)
  ;; subset of events
  (fixed-events getter: get-fixed-events)
  ;; events again, but as stream with repeating events realised
  (event-set getter: get-event-set)
  ;; hash-table from event UID:s, to the events
  uid-map
  )


(define-method (get-event-by-uid (this <events>) uid)
  (hash-ref (slot-ref this 'uid-map) uid))


(define-method (get-calendar-by-name (this <events>) string)
  (find (lambda (c) (string=? string (prop c 'NAME)))
        (get-calendars this)))


(define-method (fixed-events-in-range (this <events>) start end)
  (filter-sorted (lambda (ev) ((in-date-range? start end)
                          (as-date (prop ev 'DTSTART))))
                 (slot-ref this 'fixed-events)))


(define-method (initialize (this <events>) args)
  (next-method)

  (format (current-error-port) (_ "Building <events> from~%"))
  (for calendar in (slot-ref this 'calendar-files)
       (format (current-error-port) "  - ~a~%" calendar))

  (let ((calendars (load-calendars (slot-ref this 'calendar-files))))
    (apply add-calendars this calendars)))

(define-method (add-calendars (this <events>) . calendars)

  (slot-set! this 'calendars (append calendars (slot-ref this 'calendars)))

  (let* ((groups
          (group-by
           type (concatenate
                 (map children (slot-ref this 'calendars)))))
         (events (awhen (assoc-ref groups 'VEVENT)
                        (car it)))
         (removed remaining (partition (extract 'X-HNH-REMOVED) events)))

    ;; TODO figure out what to do with removed events

    (slot-set! this 'events (append #|removed|# remaining)))

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
    (set! (prop event 'UID) (uuid)))




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
  (slot-set! this 'events
             (delq1! event (slot-ref this 'events)))

  (let ((slot-name (if (repeating? event) 'repeating-events 'fixed-events)))
    (slot-set! this slot-name
               (delq1! event (slot-ref this slot-name))))

  (slot-set! this 'event-set
             (stream-remove
              (lambda (ev)
                (equal? (prop ev 'UID)
                        (prop event 'UID)))
              (slot-ref this 'event-set)))

  (hash-set! (slot-ref this 'uid-map) (prop event 'UID)
             #f))



(define-method (add-and-save-event (this <events>) calendar event)
  (cond
   [(get-event-by-uid this (prop event 'UID))
    => (lambda (old-event)

         ;; remove old instance of event from runtime
         (remove-event this old-event)

         ;; Add new event to runtime,
         ;; MUST be done after since the two events SHOULD share UID.
         ;; NOTE that this can emit warnings
         (add-event this calendar event)

         (set! (prop event 'LAST-MODIFIED)
           (current-datetime))

         ;; NOTE Posibly defer save to a later point.
         ;; That would allow better asyncronous preformance.

         ;; save-event sets -X-HNH-FILENAME from the UID. This is fine
         ;; since the two events are guaranteed to have the same UID.
         (unless ((@ (vcomponent formats vdir save-delete) save-event) event)
           (throw 'misc-error (_ "Saving event to disk failed.")))


         (unless (eq? calendar (parent old-event))
           ;; change to a new calendar
           (format (current-error-port)
                   (_ "Unlinking old event from ~a~%")
                   (prop old-event '-X-HNH-FILENAME))
           ;; NOTE that this may fail, leading to a duplicate event being
           ;; created (since we save beforehand). This is just a minor problem
           ;; which either a better atomic model, or a propper error
           ;; recovery log would solve.
           ((@ (vcomponent formats vdir save-delete) remove-event) old-event))


         (format (current-error-port)
                 (_ "Event updated ~a~%") (prop event 'UID)))]

   [else
    (add-event this calendar event)

    (set! (prop event 'LAST-MODIFIED) (current-datetime))

    ;; NOTE Posibly defer save to a later point.
    ;; That would allow better asyncronous preformance.
    (unless ((@ (vcomponent formats vdir save-delete) save-event) event)
      (throw 'misc-error (_ "Saving event to disk failed.")))

    (format (current-error-port)
            (_ "Event inserted ~a~%") (prop event 'UID))]))

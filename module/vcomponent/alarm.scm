(define-module (vcomponent alarm)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util type)
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module (vcomponent type duration)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :export (alarm-triggers))

;;; ACTION:{AUDIO,DISPLAY,EMAIL,other}

;;; Get all triggers of an alarm in reference to a given instance of
;;; an event.
;;; Timezone is the users configured timezone.
;;; The standard specifies that it is to be used for events with a date
;;; (rather than a date-time) start, but it's also used for events
;;; with a date-time start without explicit an timezone.
;;; TODO what does this return?
(define (alarm-triggers timezone event alarm)
  (typecheck event (or vevent? vtodo?))
  (typecheck alarm valarm?)

  (let* ((trigger-vline (car (prop% alarm 'TRIGGER)))
         (trigger (vline-value trigger-vline)))
    (let ((base
           (cond ((utc-datetime? trigger)
                  trigger)
                 ((duration? trigger)
                  (case (string->symbol (or (param trigger-vline 'RELATED) "START"))
                    ((START)
                     (datetime+/zoneinfo
                      (prop1 event 'DTSTART)
                      trigger))

                    ((END)
                     ;; vevent: then DTEND or (DTSTART and DURATION) MUST be present
                     ;; vtodo:  then DUE   or (DTSTART and DURATION) MUST be present
                     (cond ((or (prop1 event 'DTEND)
                                (prop1 event 'DUE))
                            => (lambda (end)
                                 (datetime+
                                  (ensure-zoned-datetime timezone end)
                                  trigger)))
                           ((prop1 event 'DURATION)
                            => (lambda (dur)
                                 (-> (ensure-zoned-datetime timezone (prop1 event 'DTSTART))
                                     (datetime+/zoneinfo dur)
                                     (datetime+/zoneinfo trigger))))
                           (else (scm-error
                                  'type-error "alarm-triggers"
                                  "Encountered ~a with neither DTEND/DUE or DURATION, with alarm relative end: ~s"
                                  (list (type event) event) #f))))

                    (else (scm-error 'type-error "alarm-triggers"
                                     "Unknown RELATED parameter: ~s"
                                     (list (param trigger-vline 'RELATED)) #f))))

                 (else (scm-error
                        'type-error "alarm-triggers"
                        "Invalid type for alarm trigger. Expected utc-datetime or duration, got ~s"
                        (list trigger) #f)))))

      (cond ((prop1 event 'REPEAT)
             => (lambda (repeat)
                  (let ((_ increment (duration->datetime (prop1 event 'DURATION))))
                    (let loop ((base base)
                               (repeat repeat))
                      (if (zero? repeat)
                          (list base)
                          (cons base (loop (datetime+/zoneinfo base increment)
                                           (1- repeat))))))))
            (else (list base))))))

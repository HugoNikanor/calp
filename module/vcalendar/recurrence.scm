(define-module (vcalendar recurrence)
  #:use-module (vcalendar)
  #:use-module (vcalendar recurrence generate)
  #:re-export (generate-recurrence-set)
  #:export (repeating?))

;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (attr ev 'RRULE)
      (attr ev 'RDATE)))

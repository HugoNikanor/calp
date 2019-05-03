(define-module (vcomponent recurrence)
  #:use-module (vcomponent base)
  #:use-module (vcomponent recurrence generate)
  #:re-export (generate-recurrence-set)
  #:export (repeating?))

;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (attr ev 'RRULE)
      (attr ev 'RDATE)))

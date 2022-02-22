(define-module (vcomponent recurrence display common)
  :use-module ((datetime) :select (locale-month))
  :export (rrule-month->string))

(define (rrule-month->string n)
  (locale-month n))

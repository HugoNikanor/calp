(define-module (vcomponent recurrence display)
  :use-module (vcomponent recurrence display common)
  :use-module (hnh util language)
  :use-module (srfi srfi-88)
  :re-export (rrule-month->string)
  :export (format-recurrence-rule))

(define* (format-recurrence-rule rrule optional: (language (resolve-language)))
  ((module-ref (resolve-interface `(vcomponent recurrence display ,language))
               'format-recurrence-rule)
   rrule))

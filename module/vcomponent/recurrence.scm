(define-module (vcomponent recurrence)
  #:use-module (vcomponent recurrence generate-alt)
  #:use-module (vcomponent recurrence parse)
  #:use-module (vcomponent recurrence internal)
  #:re-export (generate-recurrence-set
               parse-recurrence-rule
               repeating? format-recur-rule make-recur-rule))

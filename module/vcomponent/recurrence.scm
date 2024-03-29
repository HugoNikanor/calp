(define-module (vcomponent recurrence)
  :use-module (vcomponent recurrence generate)
  :use-module (vcomponent recurrence parse)
  :use-module (vcomponent recurrence internal)
  :re-export (generate-recurrence-set
              parse-recurrence-rule
              repeating? make-recur-rule))

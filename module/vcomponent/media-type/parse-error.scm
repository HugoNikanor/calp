(define-module (vcomponent media-type parse-error)
  :use-module (srfi srfi-88)
  :export (raise-calendar-parse-error))

(define* (raise-calendar-parse-error
          key: type value msg args)
  (call/cc
   (lambda (cont)
     (throw 'calendar-parse-error cont
            type value msg args))))

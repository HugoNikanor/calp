(define-module (vcomponent util instance)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (calp translation)
  :use-module ((vcomponent util instance methods) :select (make-instance))
  :export (global-event-object reload)
)





;; TODO this is loaded on compile, meaning that Guile's auto-compiler may
;; evaluate this to early.
(define-once global-event-object
  (make-instance ((@ (vcomponent config) calendar-files))))

(define* (reload optional: (files ((@ (vcomponent config) calendar-files))))
  (begin (set! global-event-object (make-instance files))
         (format (current-error-port) (G_ "Reload done~%"))))

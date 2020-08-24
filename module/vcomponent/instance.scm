(define-module (vcomponent instance)
  :use-module (calp util)
  :use-module ((calp util config) :select (get-config))
  :use-module ((oop goops) :select (make))
  :export (global-event-object)
)





;; TODO this is loaded on compile, meaning that Guile's auto-compiler may
;; evaluate this to early.
(define-once global-event-object
  (make (@@ (vcomponent instance methods) <events>)
    calendar-files: (get-config 'calendar-files)))

(define-public (reload)
  (let ((new-value (make (@@ (vcomponent instance methods) <events>)
                     calendar-files: (get-config 'calendar-files))))
    (display "Reload done\n" (current-error-port))
    (set! global-event-object new-value)))

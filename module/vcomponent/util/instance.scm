(define-module (vcomponent util instance)
  :use-module (hnh util)
  :use-module ((calp util config) :select (get-config))
  :use-module ((oop goops) :select (make))
  :use-module (calp translation)
  :export (global-event-object)
)





;; TODO this is loaded on compile, meaning that Guile's auto-compiler may
;; evaluate this to early.
(define-once global-event-object
  (make (@@ (vcomponent util instance methods) <events>)
    calendar-files: (get-config 'calendar-files)))

(define-public (reload)
  (let ((new-value (make (@@ (vcomponent util instance methods) <events>)
                     calendar-files: (get-config 'calendar-files))))
    (format (current-error-port) (_ "Reload done~%"))
    (set! global-event-object new-value)))

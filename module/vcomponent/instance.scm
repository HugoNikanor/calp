(define-module (vcomponent instance)
  :use-module (util)
  :use-module ((util config) :select (get-config))
  :use-module ((oop goops) :select (make))
  :export (global-event-object)
)





;; this is loaded on compile, meaning that Guile's auto-compiler may
;; evaluate this to early.
(define-once global-event-object
  (make (@@ (vcomponent instance methods) <events>)
    calendar-files: (get-config 'calendar-files)))

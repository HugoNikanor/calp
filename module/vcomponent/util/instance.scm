(define-module (vcomponent util instance)
  :use-module (hnh util)
  :use-module (calp translation)
  :use-module ((vcomponent util instance methods) :select (make-instance))
  :export (global-event-object)
)





;; TODO this is loaded on compile, meaning that Guile's auto-compiler may
;; evaluate this to early.
(define-once global-event-object
  (make-instance ((@ (vcomponent config) calendar-files))))

(define-public (reload)
  (begin (set! global-event-object (make-instance ((@ (vcomponent config) calendar-files))))
         (format (current-error-port) (_ "Reload done~%"))))

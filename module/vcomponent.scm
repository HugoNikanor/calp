(define-module (vcomponent)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module (vcomponent load)
  :use-module (util)
  :re-export (make-vcomponent
              parse-cal-path parse-calendar
              load-calendars load-calendars*))

(re-export-modules (vcomponent base))

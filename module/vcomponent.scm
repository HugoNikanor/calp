(define-module (vcomponent)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module (util)
  :re-export (make-vcomponent parse-cal-path parse-calendar))

(re-export-modules (vcomponent base))


(define-module (vcomponent formats ical)
  :use-module ((vcomponent formats ical output)
               :select (component->ical-string))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :export (serialize
           deserialize
           )
   )


(define (serialize component port)
  (display (component->ical-string component)
           port))

(define (deserialize port)
  (parse-calendar port)
  )

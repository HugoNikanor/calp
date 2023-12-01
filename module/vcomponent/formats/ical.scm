(define-module (vcomponent formats ical)
  :use-module ((vcomponent formats ical output)
               :select (component->ical-string))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :export (serialize
           deserialize))


(define (serialize component port)
  (with-output-to-port port
    (lambda () (component->ical-string component))))

(define (deserialize port)
  (parse-calendar port))

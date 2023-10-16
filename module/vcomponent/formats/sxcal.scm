(define-module (vcomponent formats sxcal)
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal))
  :export (serialize deserialize))


(define (serialize component port)
  (write (serialize/object component) port))

(define (serialize/object component)
  (vcomponent->sxcal component))

(define (deserialize port)
  (sxcal->vcomponent port))

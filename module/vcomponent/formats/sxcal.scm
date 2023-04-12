(define-module (vcomponent formats sxcal)
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :export (serialize deserialize)
  )


(define (serialize component port)
  (write (serialize/object component) port))

(define (serialize/object component)
  ;; TODO where is this defined?
  (vcomponent->sxcal component))

(define (deserialize port)
  (sxcal->vcomponent port))

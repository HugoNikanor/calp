(define-module (vcomponent formats sxcal)
               :export (serialize deserialize)
               )


(define (serialize component port)
  'TODO
  )

(define (deserialize port)
  (sxcal->vcomponent port))

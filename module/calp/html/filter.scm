(define-module (calp html filter)
  :use-module (calp util config)
  )

(define-config summary-filter (lambda (_ a) a)
  pre: (ensure procedure?))

(define-config description-filter (lambda (_ a) a)
  pre: (ensure procedure?))

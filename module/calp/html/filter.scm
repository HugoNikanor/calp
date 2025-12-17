(define-module (calp html filter)
  :use-module (calp util config)
  :use-module (calp translation)
  )

(define-config summary-filter (with-source (lambda (_ a) a))
  pre: (ensure procedure?)
  description:
  (G_ "Transforms the event summary in HTML output"))

(define-config description-filter (with-source (lambda (_ a) a))
  pre: (ensure procedure?)
  description:
  (G_ "Transforms the description body in HTML output."))

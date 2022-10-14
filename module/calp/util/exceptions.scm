(define-module (calp util exceptions)
  :use-module (calp util config)
  :use-module (calp translation)
  :use-module (hnh util exceptions))

(define-config warnings-are-errors #f
  description: (G_ "Crash on warnings.")
  post: (@ (hnh util exceptions) warnings-are-errors)
  )

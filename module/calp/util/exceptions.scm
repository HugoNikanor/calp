(define-module (calp util exceptions)
  :use-module (calp util config)
  :use-module (calp translation)
  :use-module (hnh util exceptions))

(define-config warnings-are-errors #f
  description: (_ "Crash on warnings.")
  post: warnings-are-errors)

(define-module (calp util exceptions)
  :use-module (calp util config)
  :use-module (hnh util exceptions))

(define-config warnings-are-errors #f
  description: "Crash on warnings."
  post: warnings-are-errors)

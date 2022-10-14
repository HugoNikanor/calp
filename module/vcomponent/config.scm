(define-module (vcomponent config)
  :use-module (hnh util)
  :use-module (calp translation)
  :use-module (calp util config))

(define-config calendar-files '()
  description: (G_ "Which files to parse. Takes a list of paths or a single string which will be globbed.")
  pre: (lambda (v)
         (cond [(list? v) v]
               [(string? v) ((@ (glob) glob) v)]
               [else #f])))

(define-config default-calendar ""
  description: (G_ "Default calendar to use for operations. Set to empty string to unset")
  pre: (ensure string?))


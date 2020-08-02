(define-module (vcomponent)
  :use-module (util)
  :use-module (util config)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module (vcomponent instance methods)
  :re-export (make-vcomponent
              parse-cal-path parse-calendar))

(re-export-modules (vcomponent base)
                   (vcomponent instance methods))

(define-config calendar-files '()
  "Which files to parse. Takes a list of paths or a single string which will be globbed."
  pre: (lambda (v)
         (cond [(list? v) v]
               [(string? v) ((@ (glob) glob) v)]
               [else #f])))


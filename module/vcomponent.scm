(define-module (vcomponent)
  :use-module (calp util)
  :use-module (calp util config)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module (vcomponent instance methods)
  :re-export (make-vcomponent
              parse-cal-path parse-calendar))

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(vcomponent base)))
(module-use! cm (resolve-interface '(vcomponent instance methods)))


(define-config calendar-files '()
  description: "Which files to parse. Takes a list of paths or a single string which will be globbed."
  pre: (lambda (v)
         (cond [(list? v) v]
               [(string? v) ((@ (glob) glob) v)]
               [else #f])))

(define-config default-calendar ""
  description: "Default calendar to use for operations. Set to empty string to unset"
  pre: (ensure string?))


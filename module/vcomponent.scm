(define-module (vcomponent)
  :use-module (calp util)
  :use-module (calp util config)
  :use-module (vcomponent base)
  ;; :use-module ((vcomponent util instance methods)
  ;;              :select (make-vcomponent))
  :use-module ((vcomponent util parse-cal-path)
               :select (parse-cal-path))
  :re-export (make-vcomponent parse-cal-path))

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(vcomponent base)))
(module-use! cm (resolve-interface '(vcomponent util instance methods)))


(define-config calendar-files '()
  description: "Which files to parse. Takes a list of paths or a single string which will be globbed."
  pre: (lambda (v)
         (cond [(list? v) v]
               [(string? v) ((@ (glob) glob) v)]
               [else #f])))

(define-config default-calendar ""
  description: "Default calendar to use for operations. Set to empty string to unset"
  pre: (ensure string?))


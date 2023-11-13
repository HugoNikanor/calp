(define-module (vcomponent)
  :use-module (hnh util)
  :use-module (vcomponent base)
  :use-module (vcomponent config)
  :use-module ((vcomponent util parse-cal-path)
               :select (parse-cal-path))
  :re-export (
              vcomponent
              vcomponent?
              vcomponent-equal?
              set-properties
              properties
              children
              type
              extract
              extract*
              prop
              prop*
              parse-cal-path
              param
              ;; value
              vline?
              vline-parameters
              ;; configuration items
              calendar-files default-calendar))

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(vcomponent base)))
(module-use! cm (resolve-interface '(vcomponent util instance methods)))


(define-module (vcomponent)
  :use-module (hnh util)
  :use-module (vcomponent base)
  :use-module (vcomponent config)
  ;; :use-module ((vcomponent util instance methods)
  ;;              :select (make-vcomponent))
  :use-module ((vcomponent util parse-cal-path)
               :select (parse-cal-path))
  :re-export (make-vcomponent
              parse-cal-path
              ;; configuration items
              calendar-files default-calendar))

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(vcomponent base)))
(module-use! cm (resolve-interface '(vcomponent util instance methods)))


;;; Primitive export of symbols linked from C binary.

(define-module (vcomponent primitive)
  #:export (make-vcomponent
            add-line! add-child!
            make-vline add-attribute!
            parse-cal-path))

(load-extension "libguile-calendar" "init_lib")

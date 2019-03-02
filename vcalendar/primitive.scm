;;; Primitive export of symbols linked from C binary.

(define-module (vcalendar primitive)
  #:export (%vcomponent-children
            %vcomponent-push-child!

            %vcomponent-parent

            %vcomponent-make
            %vcomponent-type

            %vcomponent-set-attribute!
            %vcomponent-get-attribute

            %vcomponent-attribute-list))

(setenv "LD_LIBRARY_PATH" (dirname (dirname (current-filename))))
(load-extension "libguile-calendar" "init_lib")

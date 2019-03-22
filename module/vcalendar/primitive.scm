;;; Primitive export of symbols linked from C binary.

(define-module (vcalendar primitive)
  #:export (%vcomponent-children
            %vcomponent-push-child!
            %vcomponent-filter-children!

            %vcomponent-parent

            %vcomponent-make
            %vcomponent-type

            %vcomponent-set-attribute!
            %vcomponent-get-attribute

            %vcomponent-attribute-list

            %vcomponent-shallow-copy))

(setenv "LD_LIBRARY_PATH"
        (string-append (dirname (dirname (dirname (current-filename))))
                       "/lib"))
(load-extension "libguile-calendar" "init_lib")

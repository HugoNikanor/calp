;;; Primitive export of symbols linked from C binary.

(define-module (vcalendar primitive)
  #:export (%vcomponent-children
            %vcomponent-push-child!
            %vcomponent-filter-children!

            %vcomponent-parent

            %vcomponent-make
            %vcomponent-get-type
            %vcomponent-set-type!

            %vcomponent-set-attribute!
            %vcomponent-get-attribute

            %vcomponent-get-property
            %vcomponent-property-list

            %vcomponent-attribute-list

            %vcomponent-shallow-copy))

(setenv "LD_LIBRARY_PATH"
        (string-append (dirname (dirname (dirname (current-filename))))
                       "/lib"))
(load-extension "libguile-calendar" "init_lib")

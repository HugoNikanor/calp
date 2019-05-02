;;; Primitive export of symbols linked from C binary.

(define-module (vcomponent primitive)
  #:export (%vcomponent-children
            %vcomponent-push-child!
            ;; %vcomponent-filter-children!

            %vcomponent-parent

            %vcomponent-make
            %vcomponent-get-type
            %vcomponent-set-type!

            %vcomponent-get-attribute
            %vcomponent-attribute-list

            %vcomponent-shallow-copy))

(setenv "LD_LIBRARY_PATH"
        (string-append (dirname (dirname (dirname (current-filename))))
                       "/lib"))
(load-extension "libguile-calendar" "init_lib")

(define (%vcomponent-attribute-list comp)
  (map car (hash-map->list cons (%vcomponent-get-hash-table comp))))

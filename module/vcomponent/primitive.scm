;;; Primitive export of symbols linked from C binary.

(define-module (vcomponent primitive)
  #:export #; 
  (%vcomponent-children                 ;
  %vcomponent-push-child!               ;
  %vcomponent-filter-children!          ;
                                        ;
  %vcomponent-parent                    ;
                                        ;
  %vcomponent-make                      ;
  %vcomponent-get-type                  ;
  %vcomponent-set-type!                 ;
                                        ;
  %vcomponent-get-attribute             ;
  %vcomponent-attribute-list            ;
                                        ;
  %vcomponent-shallow-copy)

  (make-vcomponent add-line! add-child! make-vline add-attribute! parse-cal-path)
  )

(load-extension "libguile-calendar" "init_lib")

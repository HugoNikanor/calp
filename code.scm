(begin
 (setenv "LD_LIBRARY_PATH" (getcwd))
 (load-extension "libguile-calendar" "init_calendar")
 (define v (make-calendar "cal")))

(get-attr v 0 "description")

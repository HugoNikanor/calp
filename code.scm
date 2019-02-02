#!/usr/bin/guile \
-s
!#

(use-modules (ice-9 format))

(begin
  ;; Supurflous begin block here to make sourcing into geiser easier.
  (setenv "LD_LIBRARY_PATH" (getcwd))
  (load-extension "libguile-calendar" "init_calendar")
  (define v (make-calendar "cal")))

(do ((i 0 (1+ i)))
    ((>= i (calendar-size v)))
  (format #t "~3d | ~a~%"
          i (calendar-get-attr v i "summary")))



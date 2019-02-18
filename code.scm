#!/usr/bin/guile \
-s
!#

(add-to-load-path (dirname (current-filename)))
(load "helpers.scm")

(use-modules (ice-9 format)
             (ice-9 pretty-print))

(begin
  ;; Supurflous begin block here to make sourcing into geiser easier.
  (setenv "LD_LIBRARY_PATH" (getcwd))
  (load-extension "libguile-calendar" "init_vcomponent"))

(begin
  (define root (make-vcomponent "test.ics"))
  (define cal (car (vcomponent-children root)))
  (define events (vcomponent-children cal)))

(define (pp-list strs)
  (for-each (lambda (i str)
              (format #t "~3d | ~a~%"
                      (1+ i)
                      str))
            (iota (length strs))
            strs))

(pp-list
 (map (lambda (c) (car (vcomponent-get-attribute c "summary")))
      events))

#;
(do ((i 0 (1+ i)))
    ((>= i (calendar-size v)))
  (format #t "~3d | ~a~%"
          (1+ i) (car (calendar-get-attr v i "summary"))))


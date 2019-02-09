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
  (load-extension "libguile-calendar" "init_calendar"))

(define (get-properties calendar)
  "Fancy wrappen around primitive-get-properties.
Transforms character codes into actuall chcaracters."
  (define (recur tree)
    (let ((head (car tree)))
      (cons (cons (integer->char (car head))
                  (cdr head))
            (map recur (cdr tree)))))
  (let ((result (recur (primitive-get-properties calendar))))
    (cons 'ROOT (cdr result))))

(let* ((v (make-calendar "test-cal/alarm"))
       (props (get-properties (cadr (get-components (car (get-components v)))))))
  (pretty-print (beautify (flatten props)))
  (newline))

#;
(do ((i 0 (1+ i)))
    ((>= i (calendar-size v)))
  (format #t "~3d | ~a~%"
          (1+ i) (car (calendar-get-attr v i "summary"))))


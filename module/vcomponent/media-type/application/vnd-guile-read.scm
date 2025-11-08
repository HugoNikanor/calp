;;; Should really be application/vnd.guile-read, but periods aren't
;;; allowed in guile module names.
(define-module (vcomponent media-type application vnd-guile-read)
  :use-module (vcomponent media-type)
  :export (format))

(define format
  (calendar-data-format
   serializer: write
   parser: (lambda (p)
             (with-fluids ((read-eval? #t))
               (read p)))))

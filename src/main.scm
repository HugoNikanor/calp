#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (parse))

(define (main args)

;; (define *path* "/home/hugo/.local/var/cal/STABEN/599ca4a2f8eda362aaac598c999321dcc8004780a1d5cef36019c7e421b70b08.ics")
;; (define root (parse-cal-path *path*))

;; (format #t "root = ~a~%" root)


  (format (current-error-port) "Parsing ~s~%" (cadr args))
  (let ((cal (read-tree (cadr args))))
    (format #t "cal = ~a~%" cal)
    (format (current-error-port) "~a events~%" (length cal)))

  )



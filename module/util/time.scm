(define-module (util time)
  :export (report-time!))


(define report-time!
  (let ((last 0))
   (lambda (fmt . args)
     (let ((run (get-internal-run-time))
                                        ; (real (get-internal-real-time))
           )
       (format (current-error-port) "~7,4fs (+ ~,4fs) │ ~?~%"
               (/ run internal-time-units-per-second)
               (/ (- run last) internal-time-units-per-second)
               ;; (/ real internal-time-units-per-second)
               fmt args)
       (set! last run)))))

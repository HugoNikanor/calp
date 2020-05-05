(define-module (util time)
  :use-module (ice-9 match)
  :export (report-time! profile!))


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

(define-macro (profile! proc)
  (let ((qualified-procedure
         (match proc
           [((or '@ '@@) (module ...) symb)
            `(@@ ,module ,symb)]
           [symb
            `(@@ ,(module-name (current-module)) ,symb)]))
        (og-procedure (gensym "proc")))
    `(let ((,og-procedure ,qualified-procedure))
       (set! ,qualified-procedure
         (let ((accumulated-time 0)
               (count 0))
          (lambda args
            (set! count (1+ count))
            (let ((start-time (gettimeofday)))
              (let ((return (apply ,og-procedure args)))
                (let ((end-time (gettimeofday)))
                  (let ((runtime (+ (- (car end-time) (car start-time))
                                    (/ (- (cdr end-time) (cdr start-time))
                                       1e6))))
                    (set! accumulated-time (+ accumulated-time runtime))
                    (when (> accumulated-time 1)
                      (display (format #f "~8,4fs │ ~a  (~a)~%"
                                       accumulated-time
                                       (or (procedure-name ,qualified-procedure)
                                           (quote ,qualified-procedure))
                                       count)
                               (current-error-port))
                      (set! count 0)
                      (set! accumulated-time 0)))
                  return))))))
       ,og-procedure)))

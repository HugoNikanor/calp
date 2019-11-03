(define-module (output info)
  :use-module (util))

(use-modules (ice-9 getopt-long)
             (vcomponent)
             (vcomponent output)
             (vulgar color)
             (srfi srfi-1))

(define-public (info-main calendars events args)
  (format #t "~%Found ~a calendars, named:~%~{ - [~4@a] ~a~a\x1b[m~%~}~%"
          (length calendars)
          (concatenate
           (zip (map (lambda (c) (length (filter (lambda (e) (eq? 'VEVENT (type e)))
                                            (children c))))
                     calendars)
                (map (compose color-escape (extract 'COLOR)) calendars)
                (map (extract 'NAME) calendars)))))



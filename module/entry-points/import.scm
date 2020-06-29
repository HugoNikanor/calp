(define-module (entry-points import)
  :export (main)
  :use-module (util)
  :use-module (util app)
  :use-module (util options)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 rdelim)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  )

(define options
  '((calendar (value #t) (single-char #\c)
              (description "Name of calendar to import into"))
    (file (value #t) (single-char #\f)
          (description "ics file to import"))
    (help (single-char #\h)
          (description "Print this help."))))

(define (main args)
  (define opts (getopt-long args (getopt-opt options)))

  (define cal-name (option-ref opts 'calendar #f))
  (define fname (option-ref opts 'file "/dev/stdin"))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (let* ((calendars (getf 'calendars))
         (calendar
          (and cal-name
               (find (lambda (c) (string=? cal-name (prop c 'NAME)))
                     (getf 'calendars)))))

    (unless calendar
      (format (current-error-port) "No calendar named ~s~%" cal-name)
      (throw 'return))

    (let ((new-events (parse-cal-path fname)))

      (format #t "About to the following ~a events into ~a~%~{~a~^~%~}~%"
              (length (children new-events))
              (prop calendar 'NAME)
              (map (extract 'SUMMARY) (children new-events)))

      (format #t "Continue? [Y/n] ")

      (let loop ((c #\space))
        (case c
          [(#\n #\N) (throw 'return)]
          [(#\y #\Y) (map (lambda (e) (calendar-import calendar e))
                          (children new-events))]
          [else
           (let ((line (read-line)))
             (loop (if (string-null? line)
                       #\Y (string-ref line 0))))]))
      )))

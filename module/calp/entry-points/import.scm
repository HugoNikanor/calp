(define-module (calp entry-points import)
  :export (main)
  :use-module (calp util)
  :use-module (calp util options)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-1)
  ;; TODO FIX
  ;; :use-module (output vdir)
  :use-module ((vcomponent formats vdir save-delete) :select (save-event))
  :use-module (vcomponent)
  ;; :use-module ((vcomponent formats ical parse) :select (parse-cal-path))
  :use-module ((vcomponent util parse-cal-path) :select (parse-cal-path))
  :autoload (vcomponent util instance) (global-event-object)
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

  (let* ((calendars (get-calendars global-event-object))
         (calendar
          (and cal-name
               (find (lambda (c) (string=? cal-name (prop c 'NAME)))
                     (get-calendars global-event-object)))))

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
          [(#\y #\Y) (map (lambda (e)
                            (add-event calendar e)
                            (save-event e))
                          (children new-events))]
          [else
           (let ((line (read-line)))
             (loop (if (string-null? line)
                       #\Y (string-ref line 0))))]))
      )))

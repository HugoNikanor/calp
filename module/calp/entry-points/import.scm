(define-module (calp entry-points import)
  :export (main)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 format)
  :use-module (srfi srfi-1)
  :use-module ((vcomponent formats vdir save-delete) :select (save-event))
  :use-module (vcomponent)
  ;; :use-module ((vcomponent formats ical parse) :select (parse-cal-path))
  :use-module ((vcomponent util parse-cal-path) :select (parse-cal-path))
  :use-module (calp translation)
  :autoload (vcomponent util instance) (global-event-object)
  )

(define options
  `((calendar (value #t) (single-char #\c)
              (description ,(G_ "Name of calendar to import into")))
    (file (value #t) (single-char #\f)
          (description ,(G_ "ics file to import")))
    (help (single-char #\h)
          (description ,(G_ "Print this help.")))))

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
      (format (current-error-port) (G_ "No calendar named ~s~%") cal-name)
      (throw 'return))

    (let ((new-events (parse-cal-path fname)))

      (format #t (G_ "About to import the following ~a events into ~a~%")
              (length (children new-events))
              (prop calendar 'NAME))
      (format #t "~{~a~^~%~}~%"
              (map (extract 'SUMMARY) (children new-events)))

      (format #t (G_ "Continue? [Y/n] "))

      (let loop ((line (read-line)))
        (case (if (string-null? line) 'yes (yes-no-check line))
          [(no) (throw 'return)]
          [(yes) (map (lambda (e)
                       (add-event calendar e)
                       (save-event e))
                     (children new-events))]
          [else (loop (read-line))])))))

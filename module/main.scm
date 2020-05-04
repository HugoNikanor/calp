;; -*- geiser-scheme-implementation: guile -*-

(when (current-filename)
  (add-to-load-path (dirname (current-filename))))

(set! (@ (global) basedir) (car %load-path))

(use-modules (srfi srfi-1)
             (srfi srfi-41)
             (srfi srfi-41 util)
             (srfi srfi-88)             ; keyword syntax

             (util)
             (util io)
             (util time)
             (util app)
             (util config)
             (util options)
             ((util hooks) :select (shutdown-hook))

             ((entry-points html)      :prefix      html-)
             ((entry-points terminal)  :prefix  terminal-)
             ((entry-points import)    :prefix    import-)
             ((entry-points text)      :prefix      text-)
             ((entry-points info)      :prefix      info-)
             ((entry-points ical)      :prefix      ical-)
             ((entry-points benchmark) :prefix benchmark-)

             ((entry-points server)   :prefix   server-)

             (ice-9 getopt-long)

             (statprof)
             (repl)

             )


(define options
  '((statprof (value display-style)
              (description (*TOP* "Run the program within Guile's built in statical"
                                  "profiler. Display style is one of "
                                  (b "flat") " and " (b "tree") ".")))
    (repl (value address)
          (description
           (*TOP* "Start a Guile repl which can be connected to, defaults to the unix socket "
                  (i "/run/user/${UID}/calp-${PID}") ", but it can be bound to any unix or "
                  "TCP socket. ((@ (util app) current-app)) should return the current app context."
                  (br)
                  (b "Should NOT be used in production.")))
          )
    (help (single-char #\h)
          (description "Print this help"))))

(define module-help
  '(*TOP* (br)
    (center (b "Calp")) (br) (br)
    "Usage: " (b "calp") " [ " (i flags) " ] " (i mode) " [ " (i "mode flags") " ]" (br)

    (hr)
    (center (b "Modes")) (br) (br)

    (p (b "html") " reads calendar files from disk, and writes them to static HTML files.")

    (p (b "terminal") " loads the calendars, and startrs an interactive terminal interface.")

    "[UNTESTED]" (br)
    (p (b "import") "s an calendar object into the database.")

    (p (b "text") " formats and justifies what it's given on standard input, "
       "and writes it to standard output. Similar to this text.")

    (p (b "info") " does something?")

    (p (b "ical") " loads the calendar database, and imideately "
       "reserializes it back into ICAL format. "
       "Useful for merging calendars.")

    (p (b "benchmark") " does something else?")

    (p (b "server") " starts an HTTP server which dynamicly loads and displays event. The "
       (i "/month/{date}.html") " & " (i "/week/{date}.html") " runs the same output code as "
       (b "html") ". While the " (i "/calendar/{uid}.ics") " uses the same code as " (b "ical") ".")

    (hr) (br)
    (center (b "Flags")) (br)))

(define (ornull a b)
  (if (null? a)
      b a))


(define (wrapped-main args)
  (define opts (getopt-long args (getopt-opt options) #:stop-at-first-non-option #t))
  (define stprof (option-ref opts 'statprof #f))
  (define repl (option-ref opts 'repl #f))

  (awhen (option-ref opts 'help #f)
         (display (sxml->ansi-text module-help)
                  (current-output-port))
         (print-arg-help options)
         (throw 'return)
         )

  (when stprof (statprof-start))

  (cond [(eqv? #t repl) (repl-start (format #f "~a/calp-~a" (runtime-dir) (getpid)))]
        [repl => repl-start])

  (let ((config-file (format #f "~a/.config/calp/config.scm"
                             (getenv "HOME"))))
    (when (file-exists? config-file)
     (primitive-load config-file)))


  ;; (current-app (make-app))

  ((@ (vcomponent) init-app) (get-config 'calendar-files))
  ((@ (datetime app) init-app))

  (let ((ropt (ornull (option-ref opts '() '())
                      '("term"))))
    ((case (string->symbol (car ropt))
       ((html)       html-main)
       ((term)   terminal-main)
       ((import)   import-main)
       ((text)       text-main)
       ((info)       info-main)
       ((ical)       ical-main)
       ((server)   server-main)
       ((benchmark) benchmark-main)
       (else => (lambda (s)
                  (format (current-error-port)
                          "Unsupported mode of operation: ~a~%"
                          s)
                  (exit 1))))
     ropt))

  (when stprof
    (statprof-stop)
    (statprof-display (current-error-port)
                      style: (if (boolean? stprof)
                                 'flat
                                 (string->symbol stprof)))))


(use-modules (system vm frame))

(define (main args)
  (report-time! "Program start")
  ;; ((@ (util config) print-configuration-documentation))
  (with-throw-handler #t
    (lambda () (dynamic-wind (lambda () 'noop)
                        (lambda () (catch 'return (lambda () (wrapped-main args)) values))
                        (lambda () (run-hook shutdown-hook))
                        ))
    (lambda (err . args)
      (define stack (make-stack #t))
      (with-output-to-port (current-error-port)
        (lambda ()
          (format #t "bindings = ")
          (let loop ((frame (stack-ref stack 0)))
            (when frame
              (format #t "~{~a~^ ~}" (map binding-name (frame-bindings frame)))
              (let ((event (and=> (frame-lookup-binding frame 'event)
                                  binding-ref)))
                (when event
                  (format (current-error-port) "event = ~a~%" event)
                  ((@ (vcomponent output) serialize-vcomponent)
                   event (current-error-port))))

              (loop (frame-previous frame))))
          (format #t "~%")
          )))))

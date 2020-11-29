;; -*- geiser-scheme-implementation: guile -*-
(define-module (calp main)
  :use-module (calp util)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)             ; keyword syntax

  :use-module ((calp util config) :select (set-config! get-config get-configuration-documentation))
  :use-module (calp util options)
  :use-module ((calp util hooks) :select (shutdown-hook))

  :use-module ((text markup) :select (sxml->ansi-text))
  :use-module ((calp util exceptions) :select (filter-stack))

  :use-module (ice-9 getopt-long)
  :use-module (ice-9 regex)
  :use-module ((ice-9 popen) :select (open-input-pipe))
  :use-module ((ice-9 sandbox) :select
               (make-sandbox-module all-pure-and-impure-bindings))

  :use-module (statprof)
  :use-module (calp repl)

  :use-module ((xdg basedir) :prefix xdg-)

  )


(define options
  `((statprof (value display-style)
              (description "Run the program within Guile's built in statical "
                           "profiler. Display style is one of "
                           (b "flat") " or " (b "tree") "."))
    (repl (value address)
          (description
           "Start a Guile repl which can be connected to, defaults to the unix socket "
           (i "/run/user/${UID}/calp-${PID}") ", but it can be bound to any unix or "
           "TCP socket. ((@ (vcomponent instance) global-event-object)) "
           "should contain all events."
           (br)
           (b "Should NOT be used in production.")))

    (config (value #t)
            (description
             "Path to alterantive configuration file to load instead of the default one. "))

    ;; Techical note:
    ;; Guile's getopt doesn't support repeating keys. Thereby the small jank,
    ;; and my regex hack below.
    (option (single-char #\o)
            (value #t)
            (description
             "Set configuration options, on the form "
             (i "key") "=" (i "value")
             " as if they were set in the config file. These options have "
             "priority over those from the file. "
             "Can " (i "not") " be given with an equal after --option."
             (br) "Can be given multiple times."))

    (version (single-char #\v)
             (description "Display version, which is " ,(@ (calp) version) " btw."))

    (update-zoneinfo)

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

    (p (b "ical") " loads the calendar database, and imideately "
       "reserializes it back into ICAL format. "
       "Useful for merging calendars.")

    (p (b "benchmark") " " (i "module") (br)
       "Runs the procedure 'run-benchmark' from the module (calp benchmark " (i "module") ").")

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
  (define altconfig (option-ref opts 'config #f))

  (define config-file
    (cond [altconfig
           (if (file-exists? altconfig)
               altconfig
               (throw 'option-error
                      "Configuration file ~a missing" altconfig))]
         ;; altconfig could be placed in the list below. But I want to raise an error
         ;; if an explicitly given config is missing.
         [(find file-exists?
                (list
                 (path-append (xdg-config-home) "/calp/config.scm")
                 (path-append (xdg-sysconfdir) "/calp/config.scm")))
          => identity]))

  (when stprof (statprof-start))

  (cond [(eqv? #t repl) (repl-start (format #f "~a/calp-~a"
                                            (xdg-runtime-dir)
                                            (getpid)))]
        [repl => repl-start])


  ;; Load config
  ;; Sandbox and "stuff" not for security from the user. The config script is
  ;; assumed to be "safe". Instead it's so we can control the environment in
  ;; which it is executed.
  (catch #t
    (lambda ()
      (eval
       `(begin
          (use-modules (srfi srfi-1)
                       (srfi srfi-88)
                       (datetime)
                       (vcomponent)
                       (calp util config)
                       (glob))
          ,@(with-input-from-file config-file
              (lambda ()
                (let loop ((done '()))
                  (let ((form (read)))
                    (if (eof-object? form)
                        (reverse done)
                        (loop (cons form done))))))))
       (make-sandbox-module
        `(((guile) use-modules)
          ,@all-pure-and-impure-bindings
          ))
       ))
    (lambda args
      (format (current-error-port)
              "Failed loading config file ~a~%~s~%"
              config-file
              args
              )))



  ;; NOTE this doesn't stop at first non-option, meaning that -o flags
  ;; from sub-commands might be parsed.
  (map (lambda (pair)
         (let* (((key value) (string-split (cadr pair) #\=)))
           (set-config! (string->symbol key)
                        (let ((form (call-with-input-string value read)))
                          (if (list? form)
                              (primitive-eval form)
                              form)))))
       (filter (lambda (p)
                 ;; should match `--option', as well as a single flag with any
                 ;; number of other options, as long as the last one is `o'.
                 (string-match "^-(-option|[^-]*o)$" (car p)))
               (zip args (cdr args))))

  ;; help printing moved below some other stuff to allow
  ;; print-configuration-and-return to show bound values.
  (awhen (option-ref opts 'help #f)
         (display (sxml->ansi-text module-help)
                  (current-output-port))
         (print-arg-help options)
         (display (sxml->ansi-text
                   ;; NOTE that this can only display config
                   ;; items in loaded modules.
                   ;; See scripts/get-config.scm for finding
                   ;; all configuration items.
                   (get-configuration-documentation))
                  (current-output-port))
         (throw 'return)
         )

  (when (option-ref opts 'version #f)
    (format #t "Calp version ~a~%" (@ (calp) version))
    (throw 'return))

  (when (option-ref opts 'update-zoneinfo #f)
    (let* ((locations (list "/usr/libexec/calp/tzget" (path-append (xdg-data-home) "/tzget")))
           (filename (or (find file-exists? locations)
                         (error "tzget not installed, please put it in one of ~a" locations)))
           (pipe (open-input-pipe filename)))

      ;; (define path (read-line pipe))
      (define line ((@ (ice-9 rdelim) read-line) pipe))
      (define names (string-split line #\space))
      ((@ (calp util io) with-atomic-output-to-file)
       (path-append (xdg-data-home) "/calp/zoneinfo.scm")
       (lambda ()
         (write `(set-config! 'tz-list ',names)) (newline)
         (write `(set-config! 'last-zoneinfo-upgrade ,((@ (datetime) current-date)))) (newline)))))

  ;; always load zoneinfo if available.
  (let ((z (path-append (xdg-data-home) "/calp/zoneinfo.scm")))
    (when (file-exists? z)
      (primitive-load z)))


  (let ((ropt (ornull (option-ref opts '() '())
                      '("term"))))
    ((case (string->symbol (car ropt))
       ((html)   (@ (calp entry-points     html) main))
       ((term)   (@ (calp entry-points terminal) main))
       ((import) (@ (calp entry-points   import) main))
       ((text)   (@ (calp entry-points     text) main))
       ((ical)   (@ (calp entry-points     ical) main))
       ((server) (@ (calp entry-points   server) main))
       ((convert) (@ (calp entry-points convert) main))
       ((tidsrapport) (@ (calp entry-points   tidsrapport) main))
       ((benchmark) (@ (calp entry-points benchmark) main))
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



(define-public (main args)
  ((@ (calp util time) report-time!) "Program start")
  (with-throw-handler #t
    (lambda ()
      (dynamic-wind (lambda () 'noop)
                    (lambda () (catch 'return (lambda () (wrapped-main args)) values))
                    (lambda () (run-hook shutdown-hook))))
    (lambda _
      ;; Finds any direct vcomponents (not in lists or similar) on the stack
      ;; and prints them.
      (map (lambda (it)
             (with-output-to-port (current-error-port)
               (lambda () ((@ (vcomponent describe) describe) it))))
           (filter-stack (@ (vcomponent) vcomponent?) (make-stack #t))))))

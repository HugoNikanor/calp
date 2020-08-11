;; -*- geiser-scheme-implementation: guile -*-

(when (current-filename)
  (add-to-load-path (dirname (current-filename))))

(set! (@ (global) basedir) (car %load-path))

(catch 'misc-error
  (lambda () (use-modules (autoconfig)))
  (lambda (err caller fmt args . rest)
    (if (eqv? (caadr args) 'autoconfig)
        (format (current-error-port) "Run ./configure first~%")
        (format (current-error-port) "~?~%" fmt args))
    (exit 1)))

(use-modules (srfi srfi-1)
             (srfi srfi-88)             ; keyword syntax

             (util)
             ((util config) :select (set-config! get-configuration-documentation))
             (util options)
             ((util hooks) :select (shutdown-hook))

             (text markup)

             (ice-9 getopt-long)
             (ice-9 regex)

             (statprof)
             (repl)

             )


(define options
  '((statprof (value display-style)
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

    (p (b "benchmark") " Forces a field from the current app. Preferably used together with "
       (i "--statprof") " for some for profiling the code.")

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

  (when stprof (statprof-start))

  (cond [(eqv? #t repl) (repl-start (format #f "~a/calp-~a" (runtime-dir) (getpid)))]
        [repl => repl-start])

  (if altconfig
      (begin
        (if (file-exists? altconfig)
            (primitive-load altconfig)
            (throw 'option-error "Configuration file ~a missing" altconfig)))
      ;; if not altconfig, then regular config
      (let ((config-file (format #f "~a/.config/calp/config.scm"
                                 (getenv "HOME"))))
        (when (file-exists? config-file)
          (primitive-load config-file))))


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

  (let ((ropt (ornull (option-ref opts '() '())
                      '("term"))))
    ((case (string->symbol (car ropt))
       ((html)   (@ (entry-points     html) main))
       ((term)   (@ (entry-points terminal) main))
       ((import) (@ (entry-points   import) main))
       ((text)   (@ (entry-points     text) main))
       ((ical)   (@ (entry-points     ical) main))
       ((server) (@ (entry-points   server) main))
       ((benchmark) (@ (entry-points benchmark) main))
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

(define (main args)
  ((@ (util time) report-time!) "Program start")
  (dynamic-wind (lambda () 'noop)
                (lambda () (catch 'return (lambda () (wrapped-main args)) values))
                (lambda () (run-hook shutdown-hook))))

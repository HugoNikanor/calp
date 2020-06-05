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
             ((entry-points ical)      :prefix      ical-)
             ((entry-points benchmark) :prefix benchmark-)

             ((entry-points server)   :prefix   server-)

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
           "TCP socket. ((@ (util app) current-app)) should return the current app context."
           (br)
           (b "Should NOT be used in production.")))

    ;; TODO --config flag for loading alternate configuration file.

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

  (when stprof (statprof-start))

  (cond [(eqv? #t repl) (repl-start (format #f "~a/calp-~a" (runtime-dir) (getpid)))]
        [repl => repl-start])

  (let ((config-file (format #f "~a/.config/calp/config.scm"
                             (getenv "HOME"))))
    (when (file-exists? config-file)
     (primitive-load config-file)))

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
         (display (sxml->ansi-text (get-configuration-documentation))
                  (current-output-port))
         (throw 'return)
         )
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


(define logport (make-parameter (open-file "/tmp/calp.xml" "a")))

(define (main args)

  (when (zero? (ftell (logport)))
    (format (logport) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%"))

  (format (logport) "<run><start>~a</start>~%"
          ((@ (datetime util) datetime->string)
           ((@ (datetime) current-datetime))))
  (report-time! "Program start")
  ;; ((@ (util config) print-configuration-documentation))
  (let ((stack #f))
    (catch #t
      (lambda () (dynamic-wind (lambda () 'noop)
                          (lambda () (catch 'return (lambda () (wrapped-main args)) values))
                          (lambda () (run-hook shutdown-hook))
                          ))
      (lambda (err raiser fmt . args)
        (format #t "Calp has crashed with [~a],
~?~%See ~a for full backtrace~%"
                err fmt args (port-filename (logport)))
        (format (logport) "<trace>~%<![CDATA[~%")
        (display-backtrace stack (logport))
        (format (logport) "]]></trace></run>~%"))
      (lambda _ (set! stack (make-stack #t))))))

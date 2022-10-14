;; -*- geiser-scheme-implementation: guile -*-
(define-module (calp main)
  :use-module (hnh util)
  :use-module ((hnh util path) :select (path-append))

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)             ; keyword syntax

  :use-module (hnh util options)
  :use-module ((calp util hooks) :select (shutdown-hook))

  :use-module ((text markup) :select (sxml->ansi-text))
  :use-module ((hnh util exceptions) :select (filter-stack))
  ;; calp util exceptions contains the warnings-are-errors configuration
  ;; item, this forces that to load.
  :use-module ((calp util exceptions) :select ())

  :use-module (ice-9 getopt-long)
  :use-module (ice-9 regex)
  :use-module ((ice-9 popen) :select (open-input-pipe))
  :use-module ((ice-9 sandbox) :select
               (make-sandbox-module all-pure-and-impure-bindings))

  :use-module (statprof)
  :use-module (calp repl)
  :use-module (sxml simple)

  :use-module ((xdg basedir) :prefix xdg-)

  :use-module (calp translation)
  :use-module ((calp load-config) :select (load-config find-config-file))

  :export (main)
  )





(define options
  `((statprof (value display-style)
              (description ,(xml->sxml (G_ "<group>Run the program within Guile's built in statical
profiler. Display style is one of <b>flat</b> or <b>tree</b>.</group>"))))
    (repl (value address)
          (description
           ,(xml->sxml (G_ "<group>Start a Guile repl which can be connected to, defaults to the
unix socket <i>/run/user/${UID}/calp-${PID}</i>, but it can be bound to any
unix or TCP socket. ((@ (vcomponent util instance) global-event-object)) should
contain all events.
<br/>
<b>Should NOT be used in production.</b></group>"))))

    (config (value #t)
            (description
             ,(G_ "Path to alterantive configuration file to load instead of the default one.")))

    (debug (single-char #\d)
           (description
            ,(G_ "Turns on debug mode for HTML output")))

    (edit-mode
     (description
      ,(G_ "Makes generated HTML user editable (through JS)")))

    (version (single-char #\v)
             (description ,(format #f (G_ "Display version, which is ~a btw.")
                                   (@ (calp) version))))

    (help (single-char #\h)
          (description ,(G_ "Print this help")))

    ))

(define module-help
  (xml->sxml
   (string-append
    "<group><br/>
<center><b>" "Calp" "</b></center>
<br/><br/>
" (G_ "Usage: <b>calp</b> [ <i>flags</i> ] <i>mode</i> [ <i>mode flags</i> ]") "<br/>
<hr/>"
;; Header for following list of modes of operation
    "<center><b>" (G_ "Modes") "</b></center>
<br/><br/>"
    (G_ "<p><b>html</b> reads calendar files from disk, and writes them to static HTML files.</p>")
    (G_ "<p><b>terminal</b> loads the calendars, and starts an interactive terminal interface.</p>")
    (G_ "[UNTESTED]<br/><p><b>import</b>s a calendar object into the database.</p>")
    (G_ "<p><b>text</b> formats and justifies what it's given on standard input,
and writes it to standard output. Similar to this text.</p>")
    (G_ "<p><b>ical</b> loads the calendar database, and immediately
re-serializes it back into iCAL format. Useful for merging calendars.</p>")
    (G_ "<p><b>benchmark</b> <i>module</i><br/>Runs the procedure 'run-benchmark'
from the module (calp benchmark <i>module</i>).</p>")
    (G_ "<p><b>server</b> starts an HTTP server which dynamically loads and
displays events. The <i>/month/{date}.html</i> &amp; <i>/week/{date}.html</i> runs
the same output code as <b>html</b>. While the <i>/calendar/{uid}.ics</i> uses
the same code as <b>ical</b>.</p>")
    (G_ "<p><b>update-zoneinfo</b> in theory downloads and updates our local
zoneinfo database, but is currently broken.</p>")
    "<hr/><br/>"
    ;; Header for list of available flags.
    ;; Actual list is auto generated elsewhere.
    "<center><b>" (G_ "Flags") "</b></center>
<br/></group>")))

(define (ornull a b)
  (if (null? a)
      b a))

(define (wrapped-main args)
  (define opts (getopt-long args (getopt-opt options) stop-at-first-non-option: #t))
  (define stprof (option-ref opts 'statprof #f))
  (define repl (option-ref opts 'repl #f))
  (define altconfig (option-ref opts 'config #f))

  (define config-file (find-config-file altconfig))

  (when stprof (statprof-start))

  (load-config config-file)

  (awhen (option-ref opts 'edit-mode #f)
         ((@ (calp html config) edit-mode) #t))

  (awhen (option-ref opts 'debug #f)
         ((@ (calp html config) debug) #t))


  ;; help printing moved below some other stuff to allow
  ;; print-configuration-and-return to show bound values.
  (awhen (option-ref opts 'help #f)
         (display (sxml->ansi-text module-help)
                  (current-output-port))
         (print-arg-help options)
         (throw 'return))

  (when (option-ref opts 'version #f)
    (format #t (G_ "Calp version ~a~%") (@ (calp) version))
    (throw 'return))

  ;; always load zoneinfo if available.
  (let ((z (path-append (xdg-data-home) "calp" "zoneinfo.scm")))
    (when (file-exists? z)
      (primitive-load z)))

  ;; Start repl late, since configuration items are implemented as properties,
  ;; meaning that they are thread local (and the repl lives in its own thread).
  (cond [(eqv? #t repl) (repl-start (format #f "~a/calp-~a"
                                            (xdg-runtime-dir)
                                            (getpid)))]
        [repl => repl-start])

  (let ((ropt (ornull (option-ref opts '() '())
                      '("term"))))
    ((case (string->symbol (car ropt))
       ((html)   (@ (calp entry-points     html) main))
       ;; TODO chnange term to be non-interactive term
       ;; and then add interactive-term (or similar)
       ((term)   (@ (calp entry-points terminal) main))
       ((import) (@ (calp entry-points   import) main))
       ((text)   (@ (calp entry-points     text) main))
       ((ical)   (@ (calp entry-points     ical) main))
       ((server) (@ (calp entry-points   server) main))
       ((convert) (@ (calp entry-points convert) main))
       ((tidsrapport) (@ (calp entry-points   tidsrapport) main))
       ((benchmark) (@ (calp entry-points benchmark) main))
       ((update-zoneinfo) (@ (calp entry-points update-zoneinfo) main))
       (else => (lambda (s)
                  (format (current-error-port)
                          (G_ "Unsupported mode of operation: ~a~%")
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
  ((@ (calp util time) report-time!) (G_ "Program start"))
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
               (lambda () ((@ (vcomponent util describe) describe) it))))
           (filter-stack (@ (vcomponent) vcomponent?) (make-stack #t))))))

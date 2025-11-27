;; -*- geiser-scheme-implementation: guile -*-
(define-module (calp main)
  :use-module ((hnh util) :select (awhen))
  :use-module ((hnh util path) :select (path-append file-hidden?))

  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-88) :select ()) ; keyword syntax

  :use-module (hnh util options)
  :use-module ((calp util hooks) :select (shutdown-hook))

  :use-module ((text markup) :select (sxml->ansi-text))
  :use-module ((hnh util exceptions) :select (filter-stack))
  ;; calp util exceptions contains the warnings-are-errors configuration
  ;; item, this forces that to load.
  :use-module ((calp util exceptions) :select ())

  :use-module (ice-9 getopt-long)

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
unix or TCP socket.<br/>
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
                                   (@ (calp) calp-version))))

    (help (single-char #\h)
          (description ,(G_ "Print this help")))

    ))


;; TODO change terminal to be non-interactive term
;; and then add existing as interactive-term (or similar)

;; It would be cleaner to resolve all modules here. However, the flag
;; to only load the module once it's used doesn't seem to work, meaning
;; that we have to write our own lazy loader.
;; (We want lazy-loading, in case any entry point decides to do much work top level)
(define entry-points
  (map string->symbol
       (remove file-hidden?
        ;; Using a private procedure is ugly, but it does *exactly* what we want.
        ;; Vendor it if need be
        ((@@ (scripts list) find-submodules)
         '(calp entry-points)))))


(define (module-help)
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

    (string-concatenate
     (map (lambda (entry-point)
            (define module (resolve-interface `(calp entry-points ,entry-point)))
            (format #f "<p><b>~a</b> ~a</p>"
                    entry-point
                    (module-ref module '%summary "")))
          entry-points))

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
         (display (sxml->ansi-text (module-help))
                  (current-output-port))
         (print-arg-help options)
         (throw 'return))

  (when (option-ref opts 'version #f)
    (format #t (G_ "Calp version ~a~%") (@ (calp) calp-version))
    (throw 'return))

  ;; Start repl late, since configuration items are implemented as properties,
  ;; meaning that they are thread local (and the repl lives in its own thread).
  (cond [(eqv? #t repl) (repl-start (format #f "~a/calp-~a"
                                            (xdg-runtime-dir)
                                            (getpid)))]
        [repl => repl-start])

  (let* ((ropt (ornull (option-ref opts '() '())
                       '("terminal")))
         (name (string->symbol (car ropt))))

    (cond ((memv name entry-points)
            ((module-ref (resolve-interface `(calp entry-points ,name))
                         'main)
             ropt))
          (else (format (current-error-port)
                        (G_ "Unsupported mode of operation: ~a~%")
                        name)
                (exit 1))))

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
               (lambda () (write it))))
           (filter-stack (@ (vcomponent) vcomponent?) (make-stack #t))))))

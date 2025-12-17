(define-module (calp load-config)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (hnh util path)
  :use-module (hnh util io)
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module (ice-9 sandbox)
  :export (load-config find-config-file))


(define (all-bindings-in module)
  (cons module
        (module-map (lambda (a _) a) (resolve-interface module))))

(define (load-config config-file)
 ;; Load config
 ;; Sandbox and "stuff" not for security from the user. The config script is
 ;; assumed to be "safe". Instead it's so we can control the environment in
 ;; which it is executed.

  (define forms
    (call-with-input-file config-file
      (lambda (p) (read-all read p))))

  (eval-in-sandbox
   `(begin ,@forms)
   time-limit: 30
   allocation-limit: #e10e12
   bindings: (cons* (all-bindings-in '(guile))
                    all-pure-and-impure-bindings)))


(define* (find-config-file optional: altconfig)
  (cond [altconfig
         (if (file-exists? altconfig)
             altconfig
             (scm-error 'misc-error
                        "find-config-file"
                        (G_ "Configuration file ~a missing")
                        (list altconfig)
                        #f))]
        ;; altconfig could be placed in the list below. But I want to raise an error
        ;; if an explicitly given config is missing.
        [(find file-exists?
               (let ((end '("calp" "config.scm")))
                 `(,(apply path-append (xdg-config-home) end)
                   ,@(map (lambda (sysconfdir)
                            (apply path-append sysconfdir end))
                          (xdg-config-dirs))
                   ,(apply path-append "/etc" end))))
         => identity]

        [else (scm-error 'misc-error "find-config-file"
                         "No configuration file found"
                         '() #f)])
  )

(cond-expand
  (guile-3
   (define-module (calp load-config)
     :declarative? #f))
  (else
   (define-module (calp load-config)
     )))

(use-modules (srfi srfi-1)
             (calp translation)
             (hnh util path)
             ((xdg basedir) :prefix xdg-))

(export load-config find-config-file)

(define (load-config config-file)
 ;; Load config
 ;; Sandbox and "stuff" not for security from the user. The config script is
 ;; assumed to be "safe". Instead it's so we can control the environment in
 ;; which it is executed.
 (catch #t
   (lambda () (load config-file))
   (lambda args
     (format (current-error-port)
             ;; Two arguments:
             ;; Configuration file path,
             ;; thrown error arguments
             (G_ "Failed loading config file ~a~%~s~%")
             config-file
             args
             ))))


(define (find-config-file altconfig)
  (cond [altconfig
         (if (file-exists? altconfig)
             altconfig
             (scm-error 'misc-error
                        "wrapped-main"
                        (G_ "Configuration file ~a missing")
                        (list altconfig)
                        #f))]
        ;; altconfig could be placed in the list below. But I want to raise an error
        ;; if an explicitly given config is missing.
        [(find file-exists?
               (list
                (path-append (xdg-config-home) "calp" "config.scm")
                (path-append (xdg-sysconfdir) "calp" "config.scm")))
         => identity])
  )

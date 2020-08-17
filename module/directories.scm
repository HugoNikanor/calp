(define-module (directories)
  :use-module (util)
  :use-module (util config)
  )

;; TODO possiblyy create a (system) parameter, which flips many
;; settings between being based in $HOME, and in / (or $prefix/).

(define-config path-prefix "/usr")

(define-public runtime-directory
  (or (getenv "XDG_RUNTIME_DIR")
      "/tmp"))

(define-public system-config-directory "/etc/calp")

(define-public user-config-directory
  (path-append
    (or (getenv "XDG_CONFIG_HOME")
        (and=> (getenv "HOME")
               (lambda (s) (path-append s "/.config"))))
    "/calp"))

(define (libexec%) 
  (path-append (get-config 'path-prefix)
               "/lib/calp"))

(define-syntax libexec (identifier-syntax (libexec%)))
(export libexec)

(define (data-directory%)
  (path-append
    (or (getenv "XDG_DATA_HOME")
        (path-append (get-config 'path-prefix) "/share"))
    "/calp"))

(define-syntax data-directory (identifier-syntax (data-directory%)))
(export data-directory)


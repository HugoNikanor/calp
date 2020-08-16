(define-module (directories)
  :use-module (util))

(define-public runtime-directory
  (or (getenv "XDG_RUNTIME_DIR")
      "/tmp"))

(define-public system-config-directory "/etc/calp")

(define user-config-directory
  (path-append
    (or (getenv "XDG_CONFIG_HOME")
        (and=> (getenv "HOME")
               (lambda (s) (path-append s "/.config"))))
    "/calp"))

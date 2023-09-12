;;; Commentary:
;;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
;;; Code:

(define-module (xdg basedir)
  :export (runtime-dir
           data-home config-home state-home cache-home
           data-dirs config-dirs))

;;; Check if an environment variable is set to a non-empty value.
(define (set? var)
  (cond ((getenv var)
         => (lambda (s)
              (if (string-null? s)
                  #f s)))
        (else #f)))

;;; XDG_DATA_HOME
;;; $HOME/.local/share
(define (data-home)
  (or (set? "XDG_DATA_HOME")
      (string-append (getenv "HOME") "/.local/share")))

;;; XDG_CONFIG_HOME
;;; $HOME/.config
(define (config-home)
  (or (set? "XDG_CONFIG_HOME")
      (string-append (getenv "HOME") "/.config")))

;;; XDG_STATE_HOME
;;; $HOME/.local/state
(define (state-home)
  (or (set? "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state")))

;;; XDG_DATA_DIRS
;;; colon (:) sepparated, in addition to XDG_DATA_HOME
;;; /usr/local/share/:/usr/share/
(define (data-dirs)
  (let ((str (set? "XDG_DATA_DIRS")))
    (if str
        (string-split str #\:)
        '("/usr/local/share" "/usr/share"))))

;;; XDG_CONFIG_DIRS
;;; colon (:) separated, in adddition to XDG_CONFIG_HOME
;;; /etc/xdg
(define (config-dirs)
  (let ((str (set? "XDG_CONFIG_DIRS")))
    (if str
        (string-split str #\:)
        '("/etc/xdg"))))

;;; XDG_CACHE_HOME
;;; $HOME/.cache
(define (cache-home)
  (or (set? "XDG_CACHE_HOME")
      (string-append (getenv "HOME") "/.cache")))

;;; XDG_RUNTIME_DIR
;;; Default to /tmp or /tmp/$(uid), and raise a warning
(define (runtime-dir)
  (or (set? "XDG_RUNTIME_DIR")
      (begin
        (display "WARNING: XDG_RUNTIME_DIR unset, defaulting to /tmp\n"
                 (current-error-port))
        "/tmp")))


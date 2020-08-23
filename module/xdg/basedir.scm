;;; Commentary:
;;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
;;; Code:

(define-module (xdg basedir)
  :export (sysconfdir runtime-dir
           data-home config-home cache-home
           data-dirs config-dirs))

;;; XDG_DATA_HOME
;;; $HOME/.local/share
(define (data-home)
  (or (getenv "XDG_DATA_HOME")
      (string-append (getenv "HOME") "/.local/share")))

;;; XDG_CONFIG_HOME
;;; $HOME/.config
(define (config-home)
  (or (getenv "XDG_CONFIG_HOME")
      (string-append (getenv "HOME") "/.config")))

;;; XDG_DATA_DIRS
;;; colon (:) sepparated, in addition to XDG_DATA_HOME
;;; /usr/local/share/:/usr/share/
(define (data-dirs)
  (let ((str (getenv "XDG_DATA_DIRS")))
    (if str
        (string-split str #\:)
        '("/usr/local/share" "/usr/share"))))

;;; sysconfdir
;;; /etc
;;; Techincly not part of the standard, but it's mentioned
(define (sysconfdir)
  (or (getenv "sysconfdir")
      "/etc"))


;;; XDG_CONFIG_DIRS
;;; colon (:) separated, in adddition to XDG_CONFIG_HOME
;;; /etc/xdg
(define (config-dirs)
  (let ((str (getenv "XDG_CONFIG_DIRS")))
    (if str
        (string-split str #\:)
        (list (string-append sysconfdir "/xdg")))))

;;; XDG_CACHE_HOME
;;; $HOME/.cache
(define (cache-home)
  (or (getenv "XDG_CACHE_HOME")
      (string-append (getenv "HOME") "/.cache")))

;;; XDG_RUNTIME_DIR
;;; Default to /tmp or /tmp/$(uid), and raise a warning
(define (runtime-dir)
  (or (getenv "XDG_RUNTIME_DIR")
      (begin
        (display "WARNING: XDG_RUNTIME_DIR unset, defaulting to /tmp\n"
                 (current-error-port))
        "/tmp")))

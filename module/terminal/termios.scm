;;; Module for termios interaction from Guile,
;;; Since that for some reason isn't built in.

(define-module (terminal termios)
  #:export (c-lflags-disable! c-lflag-restore!))

(define-public ECHO	#x0000010)
(define-public ICANON	#x0000002)

(setenv "LD_LIBRARY_PATH"
        (string-append (dirname (dirname (dirname (current-filename))))
                       "/lib"))
(load-extension "libtermios" "init_termios")

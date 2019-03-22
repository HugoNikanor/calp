;;; Module for terminal (ANSI) escape codes.

(define-module (terminal escape)
  #:use-module (srfi srfi-60)
  #:use-module (terminal termios)
  #:export (with-vulgar))

(define-public (cls)
  (display "\x1b[H")                    ; Move cursor to the origin
  (display "\x1b[J")                    ; Clear everything after cursor
  )

;;; I don't curse, I'm just vulgar.

(define-syntax with-vulgar
  (syntax-rules ()
    ((_ thunk)
     (let ((ifd (fileno (current-input-port)))
           (ofd (fileno (current-output-port))))
       (dynamic-wind
         (lambda ()
           (let ((bits (bitwise-ior ECHO ICANON)))
             (c-lflags-disable! ifd bits)
             (c-lflags-disable! ofd bits)))
         thunk
         (lambda ()
           (c-lflag-restore! ifd)
           (c-lflag-restore! ofd))))  )))

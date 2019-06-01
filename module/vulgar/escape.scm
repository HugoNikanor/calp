;;; Module for terminal (ANSI) escape codes.

(define-module (vulgar escape)
  #:use-module (srfi srfi-60)
  #:use-module (vulgar termios)
  #:use-module (util)
  #:export (with-vulgar))

(define-public (cls)
  (display "\x1b[H")                    ; Move cursor to the origin
  (display "\x1b[J")                    ; Clear everything after cursor
  )

;;; I don't curse, I'm just vulgar.

(define-syntax with-vulgar
  (syntax-rules ()
    ((_ thunk)
     (let* ((ifd (current-input-port))
            (ofd (current-output-port))
            (iattr (make-termios))
            (oattr (make-termios))
            iattr* oattr*)
       (dynamic-wind
         (lambda ()
           (tcgetattr! iattr ifd)
           (tcgetattr! oattr ofd)

           ;; Store current settings to enable resetting the terminal later
           (set! iattr* (copy-termios iattr))
           (set! oattr* (copy-termios oattr))

           (let ((bits (bitwise-not (bitwise-ior ECHO ICANON))))
             (set! (lflag iattr) (bitwise-and (lflag iattr) bits))
             (set! (lflag oattr) (bitwise-and (lflag oattr) bits)))

           (tcsetattr! iattr ifd)
           (tcsetattr! oattr ofd))
         thunk
         (lambda ()
           (tcsetattr! iattr* ifd)
           (tcsetattr! oattr* ofd)))))))

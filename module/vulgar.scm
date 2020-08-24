;;; Commentary:

;; I don't curse, I'm just vulgar.

;;; Code:

(define-module (vulgar)
  #:use-module (srfi srfi-60)
  #:use-module (vulgar termios)
  #:use-module (calp util)
  #:export (with-vulgar))

(define-public (cls)
  ;; [H]ome, [J]: clear everything after
  (display "\x1b[H\x1b[J"))

(define-public (set-cursor-pos x y)
  (format #t "\x1b[~a;~aH"
          (1+ y) (1+ x)))


(define-syntax with-vulgar
  (syntax-rules ()
    ((_ thunk)
     (with-vulgar (bitwise-not (bitwise-ior ECHO ICANON))
                  thunk))
    ((_ bits thunk)
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
           (set! iattr* (copy-termios iattr)
                 oattr* (copy-termios oattr)

                 (lflag iattr) (bitwise-and bits (lflag iattr))
                 (lflag oattr) (bitwise-and bits (lflag oattr)))

           (tcsetattr! iattr ifd)
           (tcsetattr! oattr ofd)
           (system "tput civis"))
         thunk
         (lambda ()
           (tcsetattr! iattr* ifd)
           (tcsetattr! oattr* ofd)
           (system "tput cnorm")
           ))))))

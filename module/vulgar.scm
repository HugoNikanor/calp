;;; Commentary:

;; I don't curse, I'm just vulgar.

;;; Code:

(define-module (vulgar)
  #:use-module (srfi srfi-60)
  #:use-module (vulgar termios)
  #:use-module (hnh util)
  #:export (cls set-cursor-pos with-vulgar))

(define (cls)
  ;; [H]ome, [J]: clear everything after
  (display "\x1b[H\x1b[J"))

(define (set-cursor-pos x y)
  (format #t "\x1b[~a;~aH"
          (1+ y) (1+ x)))


(define (with-vulgar . args)
  (apply
   (case-lambda
     ((thunk)
      (with-vulgar (bitwise-not (bitwise-ior ECHO ICANON))
                   thunk))
     ((bits thunk)
      (let ((ifd (current-input-port))
            (ofd (current-output-port))
            (iattr (make-termios))
            (oattr (make-termios))
            (iattr* #f)
            (oattr* #f))
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
            (format #t "\x1b[?1049h")
            (system "tput civis"))
          thunk
          (lambda ()
            (tcsetattr! iattr* ifd)
            (tcsetattr! oattr* ofd)
            (format #t "\x1b[?1049l")
            (system "tput cnorm")
            )))))
   args))

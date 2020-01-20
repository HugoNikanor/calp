;;; Commentary:

;; Tests that my termios function works, at least somewhat.
;; Note that this actually modifies the terminal it's run on, and might fail if
;; the terminal doesn't support the wanted modes. See termios(3).
;; It might also leave the terminal in a broken state if exited prematurely.

;;; Code:

(((util) mod!)
 ((vulgar termios)
  make-termios copy-termios
  lflag
  tcgetattr! tcsetattr!
  ECHO ICANON)
 ((srfi srfi-60)
  (bitwise-ior . ||)
  (bitwise-not . ~)
  (bitwise-and . &))
 ((guile) open-input-file))

(define tty (open-input-file "/dev/tty"))

(define-syntax-rule (&= lvalue val)
  (mod! lvalue (lambda (v) (& v val))))

(define t (make-termios))

(test-equal 0 (tcgetattr! t tty))
(define ifl (lflag t))

(define copy (copy-termios t))

#!curly-infix { (lflag t) &= (~ (|| ECHO ICANON)) }

(test-equal 0 (tcsetattr! t tty))
(test-equal (& ifl (~ (|| ECHO ICANON)))
  (lflag t))
(test-equal 0 (tcsetattr! copy tty))

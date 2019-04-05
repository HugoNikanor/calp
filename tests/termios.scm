;;; Commentary:

;; Tests that my termios function works, at least somewhat.
;; Note that this actually modifies the terminal it's run on, and might fail if
;; the terminal doesn't support the wanted modes. See termios(3).
;; It might also leave the terminal in a broken state if exited prematurely.

;;; Code:

(use-modules (terminal termios)
             ((util) :select (mod!))
             ((srfi srfi-60)
              :renamer (lambda (symb)
                         (case symb
                           ((bitwise-ior) '||)
                           ((bitwise-not) '~)
                           ((bitwise-and) '&)
                           (else symb)))))

(define-syntax-rule (&= lvalue val)
  (mod! lvalue (lambda (v) (& v val))))

(test-begin "termios")

(define t (make-termios))

(test-equal 0 (tcgetattr! t))
(define ifl (lflag t))

(define copy (copy-termios t))

#!curly-infix { (lflag t) &= (~ (|| ECHO ICANON)) }

(test-equal 0 (tcsetattr! t))
(test-equal (& ifl (~ (|| ECHO ICANON)))
  (lflag t))
(test-equal 0 (tcsetattr! copy))

(test-end "termios")

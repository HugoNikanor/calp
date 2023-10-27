(define-module (test vulgar)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (vulgar))

(test-equal "CLS"
  "\x1b[H\x1b[J"
  (with-output-to-string (lambda () (cls))))

;;; Extra with-output-to-string so escape sequences are escaped in error diff.
(test-equal "set-cursor-position"
  (with-output-to-string (lambda () (write "\x1b[21;11H")))
  (with-output-to-string (lambda () (write (with-output-to-string (lambda () (set-cursor-pos 10 20)))))))

(test-equal "with-vulgar returning its argument"
  'return
  (with-vulgar (lambda () 'return)))

;;; TODO these tests can test the termios flags outside and inside the
;;; vulgar block Checking that they are
;;; - different.
;;; - what's expected.
;;; - properly restored.
;; (test-group "with-vulgar tty attrs")

'((vulgar))

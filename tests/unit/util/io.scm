(define-module (test io)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util io))

(test-equal "read-lines"
  '("Test module tree"
    "================"
    ""
    "This directory contains test data, primarily for the module-introspection."
    ""
    "Changing any of these files requires a full re-run of all tests.")
  (call-with-input-file "tests/test-module-tree/README.md"
    read-lines))

;;; TODO how do you even unit test these?
;;; TODO with-atomic-output-to-file
;;; TODO call-with-tmpfile

;;; This is technically implementation dependant, since the
;;; string definition of a newline is unspecified by RnRs.
(test-group "displayln"
  (test-equal "Hello\n"
    (with-output-to-string
      (lambda () (displayln "Hello"))))
  (test-equal "World\n"
    (call-with-output-string
      (lambda (p) (displayln "World" p)))))

(test-group "->port"
  (test-assert (port? (->port (current-input-port))))
  (test-assert (port? (->port "Hello, World")))
  (test-error 'misc-error (->port 1)))

'((hnh util io))

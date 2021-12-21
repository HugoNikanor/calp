#!/usr/bin/guile \
--debug -s
!#

;;; Commentary:
;; Not a test, but a script that runs tests.
;; Assumes that all other .scm files in this directory are test files,
;; and should thereby follow the test-file syntax.
;; Note that the --debug flag in the (extended) shebang is REQUIRED,
;; otherwise the coverage tests do nothing.
;;
;; Each test runs in its own sandbox. This is however only to protect
;; the modules from each other, and to prevent polution of the global
;; namespace. The system IS NOT protected from the modules.
;;
;; Each test file is required to start with an s-expression on the
;; form:
;; @lisp
;; ((library binding ...) ...)
;; @end lisp
;; Which details exactly which modules should be imported. The format
;; is the same as make-sandbox-module. For example:
;; @example
;; (((c lex) lex)
;;  ((c parse) parse-lexeme-tree))
;; @end example
;; pulls in the @code{lex} procedure from @code{(c lex)}, and
;; @code{parse-lexeme-tree} from @code{(c parse)}.
;; Remaining forms in the file can be any valid scheme expression.
;; @code{define}s are allowed, but only where they would be allowed
;; inside a let form in general code (so only at the start for Guile
;; 2.2, anywhere for Guile 3.0).
;;; Code:

(eval-when (compile load)
 (define here (dirname (current-filename))))

(format #t "current filename = ~a~%" here)


(add-to-load-path (format #f "~a/module"
                          (dirname here)))

(use-modules (ice-9 ftw)
             (ice-9 sandbox)
             (ice-9 getopt-long)
             (srfi srfi-64)             ; test suite
             (srfi srfi-88)             ; suffix keywords
             (system vm coverage)
             ((calp util) :select (for awhen))
             ;; datetime introduces the reader extensions for datetimes,
             ;; which leaks into the sandboxes below.
             (datetime))

(define files
  (scandir here
           (lambda (name)
             (and (< 2 (string-length name))
                  (not (string=? name (basename (current-filename))))
                  (string=? "scm" (string-take-right name 3))))))



;; Load tests

(define (read-multiple)
  (let loop ((done '()))
    (let ((sexp (read)))
      (if (eof-object? sexp)
          (reverse done)
          (loop (cons sexp done))))))


(define options
  '((skip (value #t))
    (only (value #t))))

(define opts (getopt-long (command-line) options))
(define to-skip (call-with-input-string (option-ref opts 'skip "")
                  read))
(define only (option-ref opts 'only #f))

(when only (set! files (list only)))

(when (list? to-skip)
 (for skip in to-skip
      (test-skip skip)))

;; NOTE test-group fails if called before any test begin, since
;; (test-runner-current) needs to be a test-runner (dead or not),
;; but is initially bound to #f.
(test-begin "tests")

;; Forces all warnings to be explicitly handled by tests
((@ (calp util exceptions) warnings-are-errors) #t)

(define (run-with-coverage)
  (with-code-coverage
   (lambda ()
     (for fname in files
          (format (current-error-port) "Running test ~a~%" fname)
          (test-group
           fname
           (with-throw-handler #t
             (lambda ()
               (with-input-from-file (string-append here "/" fname)
                 (lambda ()
                   (let ((modules (read))
                         (forms (read-multiple)))
                     (eval-in-sandbox
                      `(begin ,@forms)
                      #:time-limit 60 ; larger than should be needed
                      #:allocation-limit #e10e8
                      #:module (make-sandbox-module
                                (append modules
                                        '(((srfi srfi-64) test-assert
                                           test-equal test-error
                                           test-eqv test-eq
                                           test-approximate)
                                          ((ice-9 ports) call-with-input-string)
                                          ((guile) make-struct/no-tail)
                                          )
                                        all-pure-bindings)))
                     (list fname modules forms)))))
             (lambda (err . args)
               (case err
                 ((misc-error)
                  (display-error #f (current-error-port)
                                 (car args)
                                 (cadr args)
                                 (caddr args)
                                 #f))
                 (else
                  (format (current-error-port)
                          "Test unexpectedly crashed: ~a~%" args))) )))))))

(call-with-values run-with-coverage
  (lambda (data _)
    (call-with-output-file "lcov.info"
      (lambda (port) (coverage-data->lcov data port)))))

(test-end "tests")


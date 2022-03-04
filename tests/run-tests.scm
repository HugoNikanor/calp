#!/usr/bin/bash

here=$(dirname $(realpath $0))

. "$(dirname "$here")/env"

make -C $(dirname $here) GUILE="$GUILE" go_files

exec $GUILE --debug -s "$0" "$@"
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

(eval-when (compile load eval)
 (define here (dirname (current-filename))))

(use-modules (srfi srfi-64))

(define (µs x)
  (* x #e1e6))

(define (transform-time-of-day tod)
  (+ (* (µs 1) (car tod))
     (cdr tod)))

(define (construct-test-runner)
  (define runner (test-runner-null))
  ;; end of individual test case
  (test-runner-on-test-begin! runner
    (lambda (runner)
      (test-runner-aux-value! runner (transform-time-of-day (gettimeofday)))))
  (test-runner-on-test-end! runner
    (lambda (runner)
      (case (test-result-kind runner)
        ((pass)  (display "\x1b[0;32mX\x1b[m"))
        ((fail)  (newline) (display "\x1b[0;31mE\x1b[m"))
        ((xpass) (display "\x1b[0;33mX\x1b[m"))
        ((xfail) (display "\x1b[0;33mE\x1b[m"))
        ((skip)  (display "\x1B[0;33m-\x1b[m")))
      (when (eq? 'fail (test-result-kind))
        (format #t " ~a~%" (test-runner-test-name runner))
        (cond ((test-result-ref runner 'actual-error)
               => (lambda (err) (format #t "Error: ~s~%" err)))
              (else
               (format #t "Expected: ~s~%Received: ~s~%"
                       (test-result-ref runner 'expected-value "[UNKNOWN]")
                       (test-result-ref runner 'actual-value "[UNKNOWN]"))))
        (format #t "Near ~a:~a~%~y"
                (test-result-ref runner 'source-file)
                (test-result-ref runner 'source-line)
                (test-result-ref runner 'source-form)))

      (let ((start (test-runner-aux-value runner))
            (end (transform-time-of-day (gettimeofday))))
        (when (< (µs 1) (- end start))
          (format #t "~%Slow test: ~s, took ~a~%"
                  (test-runner-test-name runner)
                  (exact->inexact (/ (- end start) (µs 1)))
                  )))))

  ;; on start of group
  (test-runner-on-group-begin! runner
    ;; count is number of #f
    (lambda (runner name count)
      (format #t "~a ~a ~a~%"
              (make-string 10 #\=)
              name
              (make-string 10 #\=))))
  (test-runner-on-group-end! runner
    (lambda (runner) (newline)))
  ;; after everything else is done
  (test-runner-on-final! runner
    (lambda (runner)
      (format #t "Guile version ~a~%~%" (version))
      (format #t "pass:  ~a~%" (test-runner-pass-count runner))
      (format #t "fail:  ~a~%" (test-runner-fail-count runner))
      (format #t "xpass: ~a~%" (test-runner-xpass-count runner))
      (format #t "xfail: ~a~%" (test-runner-xfail-count runner))
      ))

  runner)

(test-runner-factory construct-test-runner)

(use-modules (ice-9 ftw)
             (ice-9 sandbox)
             (ice-9 getopt-long)
             (srfi srfi-88)             ; suffix keywords
             (system vm coverage)
             ((hnh util) :select (for awhen))
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
((@ (hnh util exceptions) warnings-are-errors) #t)

(define (run-with-coverage)
  (with-code-coverage
   (lambda ()
     (for fname in files
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
                                        `(((srfi srfi-64)
                                           ,@(module-map
                                              (lambda (n _) n)
                                              (resolve-interface '(srfi srfi-64))))
                                          ((ice-9 ports) call-with-input-string)
                                          ((guile) make-struct/no-tail))
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
                 ((unbound-variable)
                  (let ((proc (car args))
                        (fmt (cadr args))
                        (fmt-args (caddr args)))
                    (format (current-error-port)
                            "[~a] ~?~%" proc fmt fmt-args)))
                 (else
                  (format (current-error-port)
                          "Test unexpectedly crashed [~a]: ~s~%" err args))) )))))))


(call-with-values run-with-coverage
  (lambda (data _)
    (call-with-output-file "lcov.info"
      (lambda (port) (coverage-data->lcov data port)))))

(test-end "tests")

#!/usr/bin/guile \
-s
!#

;;; Commentary:
;; Not a test, but a script that runs tests.
;; Assumes that all other .scm files in this directory are test files,
;; and should thereby follow the test-file syntax.
;; TODO document the testfile syntax.
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

(for fname in files
     (format (current-error-port) "Running test ~a~%" fname)
     (test-group
      fname
      (with-throw-handler #t
        (lambda ()
          (with-input-from-file (string-append here "/" fname)
            (lambda ()
              (let ((modules (read)))
                (eval-in-sandbox
                 `(begin ,@(read-multiple))
                 #:time-limit 60          ; larger than should be needed
                 #:allocation-limit #e10e8
                 #:module (make-sandbox-module
                           (append modules
                                   '(((srfi srfi-64) test-assert
                                      test-equal test-error
                                      test-eqv)
                                     ((ice-9 ports) call-with-input-string)
                                     ((guile) make-struct/no-tail)
                                     )
                                   all-pure-bindings)))))))
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
                     "Test unexpectedly crashed: ~a~%" args))) ))))
(test-end "tests")



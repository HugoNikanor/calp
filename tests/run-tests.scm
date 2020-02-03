#!/usr/bin/guile \
-s
!#

(eval-when (compile load)
 (define here (dirname (current-filename))))

(format #t "current filename = ~a~%" here)


(add-to-load-path (format #f "~a/module"
                          (dirname here)))


(use-modules (ice-9 ftw)
             (ice-9 sandbox)
             (srfi srfi-64)             ; test suite
             (srfi srfi-88)             ; suffix keywords
             ((util) :select (for awhen)))

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


;; TODO test-group fails if called before any test begin, since
;; (test-runner-current) needs to be a test-runner (dead or not),
;; but is initially bound to #f.
(test-begin "tests")

(awhen (member "--skip" (command-line))
       (for skip in (cdr it)
            (test-skip skip)))

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
                 #:module (make-sandbox-module
                           (append modules
                                   '(((srfi srfi-64) test-assert test-equal test-error)
                                     ((ice-9 ports) call-with-input-string)
                                     ((guile) make-struct/no-tail)
                                     )
                                   all-pure-bindings)))))))
        (lambda args (format (current-error-port)
                        "Test really crashed: ~a~%" args) ))))
(test-end "tests")



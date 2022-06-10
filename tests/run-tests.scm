#!/usr/bin/bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

here=$(dirname $(realpath $0))

. "$(dirname "$here")/env"

if [ "$DEBUG" = '' ]; then
  exec $GUILE -s "$0" "$@"
else
  exec $GUILE --debug -s "$0" "$@"
fi
!#

(format #t "current-filename = ~s~%" (current-filename))

(define here (dirname (current-filename)))
(use-modules (hnh util path))
(add-to-load-path (path-append (dirname here) "scripts"))

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (srfi srfi-88)
             (hnh util)
             (ice-9 ftw)
             (ice-9 format)
             (ice-9 pretty-print)
             (ice-9 getopt-long)
             (ice-9 match)
             (system vm coverage)
             ((all-modules) :select (fs-find))
             )




(define (µs x)
  (* x #e1e6))

(define (transform-time-of-day tod)
  (+ (* (µs 1) (car tod))
     (cdr tod)))

(define verbose? (make-parameter #f))

(define (escaped sequence string)
  (format #f "\x1b[~am~a\x1b[m" sequence string))

(define (green s)  (escaped 32 s))
(define (red s)    (escaped 31 s))
(define (yellow s) (escaped 33 s))
(define (bold s)   (escaped  1 s))

(define (construct-test-runner)
  (define runner (test-runner-null))
  ;; end of individual test case
  (test-runner-on-test-begin! runner
    (lambda (runner)
      (test-runner-aux-value! runner (transform-time-of-day (gettimeofday)))))
  (test-runner-on-test-end! runner
    (lambda (runner)
      (case (test-result-kind runner)
        ((pass)  (display (green "X")))
        ((fail)  (newline) (display (red "E")))
        ((xpass) (display (yellow "X")))
        ((xfail) (display (yellow "E")))
        ((skip)  (display (yellow "-"))))
      (when (or (verbose?) (eq? 'fail (test-result-kind)))
        (format #t " ~a~%"
                (cond ((test-runner-test-name runner)
                       (negate string-null?) => identity)
                      ((test-result-ref runner 'expected-value)
                       => (lambda (p) (with-output-to-string (lambda () (display (bold "[SOURCE]: ")) (truncated-print p))))))))
      (when (eq? 'fail (test-result-kind))
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



(define (rework-coverage data)
  (define-values (module-files module-names)
    ((@ (all-modules) all-modules-under-directory)
     (path-append (dirname here) "module")))

  (define to-drop
    (1+ (length
         (take-while (lambda (p) (not (string=? p "module")))
                     (path-split (car module-files))))))

  (define (drop-components path-list)
    (drop path-list to-drop))

  (define target-ht (make-hash-table))
  (define source-ht ((@@ (system vm coverage) data-file->line-counts) data))
  (for-each (lambda (path)
              (cond ((hash-ref source-ht path #f)
                     => (lambda (value) (hash-set! target-ht path value)))))
            (map (compose path-join drop-components path-split) module-files))

  ((@@ (system vm coverage) %make-coverage-data)
   ((@@ (system vm coverage) data-ip-counts)        data)
   ((@@ (system vm coverage) data-sources)          data)
   ((@@ (system vm coverage) data-file->procedures) data)
   target-ht))




(define option-spec
  '((skip (value #t))
    (only (value #t))
    (verbose (single-char #\v))
    (coverage (value optional))))

(define options (getopt-long (command-line) option-spec))

(define coverage-dest (option-ref options 'coverage #f))

(when (option-ref options 'verbose #f)
  (verbose? #t))



(define re (make-regexp "\\.scm$"))
(define files (map car
                   (filter (match-lambda ((filename _ 'regular)
                                          (regexp-exec re filename))
                                         (_ #f))
                           (fs-find (path-append here "test")))))

;; (format #t "Running on:~%~y~%" files)

(awhen (option-ref options 'only #f)
       (set! files (list (path-append "test" it))))

(awhen (option-ref options 'skip #f)
       (set! files (delete it files)))

((@ (hnh util exceptions) warnings-are-errors) #t)

(define finalizer
  (if coverage-dest
      (lambda (thunk)
        (define-values (coverage _) (with-code-coverage thunk))

        (let ((limited-coverage (rework-coverage coverage)))
          (call-with-output-file coverage-dest
            (lambda (port) (coverage-data->lcov limited-coverage port))))

        (format #t "Wrote coverage data to ~a~%" coverage-dest))
      (lambda (thunk) (thunk))
      ))

(test-begin "suite")
(finalizer (lambda () (for-each (lambda (f) (test-group f (load f))) files)))
(test-end "suite")

(newline)


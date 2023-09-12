#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

root=$(dirname "$(dirname "$(realpath "$0")")")
eval "$(env __PRINT_ENVIRONMENT=1 ${root}/calp)"

if [ "$DEBUG" = '' ]; then
  exec $GUILE -s "$0" "$@"
else
  exec $GUILE --debug -s "$0" "$@"
fi
!#

(unless (getenv "CALP_TEST_ENVIRONMENT")
  (format (current-error-port) "Not running in test environment, abandoning~%")
  (exit 1))

(format #t "current-filename = ~s~%" (current-filename))

(define here (dirname (current-filename)))
(use-modules (hnh util path))

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (srfi srfi-88)
             ((hnh util io) :select (call-with-tmpfile))
             (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 regex)
             ((ice-9 popen)
              :select (open-pipe*
                       close-pipe))
             ((ice-9 rdelim) :select (read-string))
             (system vm coverage)
             ((hnh module-introspection all-modules) :select (fs-find))

             (hnh test testrunner)
             )



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
    (coverage (value optional))
    (catch)))

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

;;; Catch/print-trace should intercept thrown exceptions, print them prettily with a stack trace, and then continue



(define catch/print-trace
  (if (option-ref options 'catch #f)
      (lambda (proc)
        (catch #t proc
          (case-lambda
            ((err from msg args data)
             (test-assert (format #f "~a in ~a: ~?" err from msg args)
               #f))
            (args
             (test-assert (format #f "~s" args)
               #f)))))
      (lambda (proc) (proc))))

#;
(define (catch/print-trace proc)
  (proc))

(test-begin "suite")


(define onlies
  (let %loop ((args (command-line)) (onlies '()))
    (define* (loop args key: only)
      (if only
          (%loop args (cons only onlies))
          (%loop args onlies)))
    (if (null? args)
        onlies
        (cond ((string-match "^--skip(=(.*))?$" (car args))
               => (lambda (m)
                    (cond ((match:substring m 2)
                           => (lambda (s)
                                (format #t "Skipping ~s~%" s)
                                (test-skip s)
                                (loop (cdr args))))
                          (else (format #t "Skipping ~s~%" (cadr args))
                                (test-skip (cadr args))
                                (loop (cddr args))))))
              ((string-match "^--only(=(.*))?$" (car args))
               => (lambda (m)
                    (cond ((match:substring m 2)
                           => (lambda (s)
                                (loop (cdr args)  only: s)))
                          (else (loop (cddr args) only: (cadr args))))))
              (else (loop (cdr args)))))))

(unless (null? onlies)
  (set! files
    (map (lambda (x) (path-append "test" x))
         ;; reverse only until I have built a dependency graph for tests
         (reverse onlies))))

(finalizer (lambda () (for-each (lambda (f) (catch/print-trace (lambda () (test-group f (load f)))))
                           files)))

(test-end "suite")

(newline)


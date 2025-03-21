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
             (hnh util)
             ((hnh util io) :select (call-with-tmpfile))
             (ice-9 ftw)
             (ice-9 format)
             (ice-9 pretty-print)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 regex)
             ((ice-9 popen)
              :select (open-pipe*
                       close-pipe))
             ((ice-9 rdelim) :select (read-string))
             (system vm coverage)
             ((hnh module-introspection all-modules) :select (fs-find))
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

(define (make-indent depth)
  (make-string (* 2 depth) #\space))

(define (string-replace-head s1 s2)
  (string-replace s1 s2
                  0 (string-length s2)))

(define (diff s1 s2)
  (let ((filename1 (call-with-tmpfile (lambda (p f) (display s1 p) f)))
        (filename2 (call-with-tmpfile (lambda (p f) (display s2 p) f))))
    (let ((pipe (open-pipe*
              OPEN_READ
              ;; "git" "diff" "--no-index"
              "diff"
              filename1 filename2)))
      (begin1 (begin
                (read-string pipe))
              (close-pipe pipe)))))

(define (pp form indent prefix-1)
  (let ((prefix (make-string (+ (string-length indent)
                                (string-length prefix-1))
                             #\space)))
    (string-replace-head
     (with-output-to-string
       (lambda () (pretty-print
              form
              per-line-prefix: prefix
              width: (- 79 (string-length indent)))))
     (string-append indent prefix-1))))


(define (construct-test-runner)
  (define runner (test-runner-null))
  (define depth 0)
  ;; end of individual test case
  (test-runner-on-test-begin! runner
    (lambda (runner)
      (test-runner-aux-value! runner (transform-time-of-day (gettimeofday)))))
  (test-runner-on-test-end! runner
    (lambda (runner)
      (when (verbose?) (display (make-indent depth)))
      (case (test-result-kind runner)
        ((pass)  (display (green "X")))
        ((fail)  (display (red "E")))
        ((xpass) (display (yellow "X")))
        ((xfail) (display (yellow "E")))
        ((skip)  (display (yellow "-"))))
      (when (or (verbose?) (eq? 'fail (test-result-kind)))
        (format #t " ~a~%"
                (cond ((test-runner-test-name runner)
                       (negate string-null?) => identity)
                      ((test-result-ref runner 'expected-value)
                       => (lambda (p) (with-output-to-string
                                   (lambda ()
                                     (display (bold "[SOURCE]: "))
                                     (truncated-print p width: 60))))))))
      (when (eq? 'fail (test-result-kind))
        (cond ((test-result-ref runner 'actual-error)
               => (lambda (err)
                    (if (and (list? err)
                             (= 5 (length err)))
                        (let ((err (list-ref err 0))
                              (proc (list-ref err 1))
                              (fmt (list-ref err 2))
                              (args (list-ref err 3)))
                         (format #t "~a~a in ~a: ~?~%"
                                 (make-indent (1+ depth))
                                 err proc fmt args))
                        (format #t "~aError: ~s~%" (make-indent (1+ depth)) err))))
              (else
               (let ((unknown-expected (gensym))
                     (unknown-actual (gensym)))
                 (let ((expected (test-result-ref runner 'expected-value unknown-expected))
                       (actual (test-result-ref runner 'actual-value unknown-actual)))
                   (let ((indent (make-indent (1+ depth))))
                     (if (eq? expected unknown-expected)
                         (format #t "~aAssertion failed~%" indent)
                         (begin
                           (display (pp expected indent "Expected: "))
                           (display (pp actual indent "Received: "))
                           (let ((d (diff (pp expected "" "")
                                          (pp actual "" ""))))
                             (display
                              (string-join
                               (map (lambda (line) (string-append indent "|" line))
                                    (string-split d #\newline))
                               "\n" 'suffix))))))))))
        (format #t "~aNear ~a:~a~%"
                (make-indent (1+ depth))
                (test-result-ref runner 'source-file)
                (test-result-ref runner 'source-line))
        (pretty-print (test-result-ref runner 'source-form)
                      (current-output-port)
                      per-line-prefix: (string-append (make-indent (1+ depth)) "> ")
                      ))

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
      (if (<= depth 1)
          (format #t "~a ~a ~a~%"
                  (make-string 10 #\=)
                  name
                  (make-string 10 #\=))
          (when (verbose?)
           (format #t "~a~a~%" (make-string (* depth 2) #\space) name)))
      (set! depth (1+ depth))))
  (test-runner-on-group-end! runner
    (lambda (runner)
      (set! depth (1- depth))
      (when (<= depth 1)
        (newline))))
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

#;
(define (catch/print-trace proc)
  (catch #t proc
    (case-lambda
      ((err from msg args data)
       (test-assert (format #f "~a in ~a: ~?" err from msg args)
         #f))
      (args
       (test-assert (format #f "~a (~s)" f args)
         #f)))))

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
        (cond ((string-match "^--skip(=.*)?$" (car args))
               => (lambda (m)
                    (cond ((match:substring m 1)
                           => (lambda (s)
                                (format #t "Skipping ~s~%" s)
                                (test-skip s)
                                (loop (cdr args))))
                          (else (format #t "Skipping ~s~%" (cadr args))
                                (test-skip (cadr args))
                                (loop (cddr args))))))
              ((string-match "^--only(=.*)?$" (car args))
               => (lambda (m)
                    (cond ((match:substring m 1)
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


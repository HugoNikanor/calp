#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

make calp unit-test-deps

root=$(dirname "$(realpath "$0")")
eval "$(env __PRINT_ENVIRONMENT=1 ${root}/calp)"


exec "$GUILE" --debug --no-auto-compile -e main -s "$0" "$@"
!#

;;; Commentary:
;;; Code:

(use-modules (glob)
             (system vm coverage)
             (srfi srfi-1)
             (srfi srfi-18)
             (srfi srfi-64)
             (srfi srfi-71)
             ((hnh util) :select (->> for group-by))
             (hnh util path)
             (hnh util type)
             ((hnh util lens) :select (modify cdr* car* compose-lenses))
             (hnh util object)
             (hnh util coverage)
             (ice-9 getopt-long)
             (ice-9 control)
             (ice-9 format))



(define-syntax with-mutex
  (syntax-rules ()
    ((_ mutex body ...)
     (dynamic-wind
       (lambda () (mutex-lock! mutex))
       (lambda () body ...)
       (lambda () (mutex-unlock! mutex))))))

(define-syntax-rule (begin-thread forms ...)
  (let ((thread (make-thread (lambda () forms ...))))
    (thread-start! thread)
    thread))



(define print
  (let ((lock (make-mutex)))
    (lambda (string)
      (with-mutex lock
        (display string)))))

(define (println x) (print (format #f "~a~%" x)))

(define (module->source-file module-name)
  ;; Guile has `module-filepath` built in, but that returns paths relative
  ;; to ONE of the items in %load-path, and we wont know which one.
  (realpath
   (string-append
    (path-join
     (cons "module"
           (map symbol->string module-name)))
    ".scm")))

(define current-filename (make-parameter #f))

(define (construct-test-runner)
  (define runner (test-runner-null))
  (test-runner-aux-value!
   runner
   `((thread-name . ,(thread-name (current-thread)))))
  #;
  (test-runner-on-test-begin! runner
    (lambda (runner)
      (test-runner-aux-value! runner #t)))
  (test-runner-on-test-end! runner
    (lambda (runner)
      (case (test-result-kind runner)
        ((pass)
         (if (getenv "VERBOSE")
             (println (format #f "~a SUCCEED ~a"
                              (current-filename)
                              (test-runner-test-name runner)))
             (display #\#)))
        ((fail)
         (println (format #f "~a FAIL ~a"
                          (current-filename)
                          (test-runner-test-name runner))))
        ((xpass xfail skip)
         (println (format #f "~a ~a ~a"
                          (current-filename)
                          (test-result-kind runner)
                          (test-runner-test-name runner)))))))
  runner)


(define (test-runner-processed runner)
  (+ (test-runner-pass-count runner)
     (test-runner-fail-count runner)
     (test-runner-xfail-count runner)
     (test-runner-skip-count runner)))


;;; TODO this procedure is also present (albeit embedded) in (hnh test testrunner). Make it a procedure
(define (test-runner-test-description runner)
  (format #f
          "~a (~a) ~a"
          (assoc-ref (test-runner-aux-value runner) 'thread-name)
          (test-runner-processed runner)
          (cond ((test-runner-test-name runner)
                 (negate string-null?) => identity)
                (else
                 (string-join (test-runner-group-path runner) " → ")))))

(define runners '())
(define runner-mutex (make-mutex))
(sigaction SIGINT
(lambda _
    (display
    (string-join (map test-runner-test-description runners) "\n"))))


(define-type (job)
  (jobname type: string?)
  (job-thunk type: thunk?))


(define* (maybe-with-code-coverage thunk key: (coverage? #f))
  (if coverage?
      (with-code-coverage
       (lambda () (catch #t thunk (lambda _ #f))))
      (values #f (catch #t thunk (lambda _ #f)))))

(define* (prepare-jobs test-files key: (coverage? #f))
  (for entry in test-files
       (job
        jobname: (basename entry)
        job-thunk:
        (lambda ()
          (parameterize ((current-filename (basename entry)))
            (test-begin entry)
            (with-mutex runner-mutex
              (set! runners (cons (test-runner-get) runners)))
            (let ((coverage module-names
                            (maybe-with-code-coverage
                             (lambda () (load entry))
                             coverage?: coverage?)))
              (let ((tested-files (map module->source-file module-names)))
                (test-end entry)
                (println (format #f "Completed ~a" entry))
                (if coverage
                    (let ((lcov-data
                           (call-with-output-string
                             (lambda (port) (coverage-data->lcov coverage port)))))
                      (filter (lambda (coverage)
                                (member (filename coverage)
                                        tested-files))
                              (parse-coverage lcov-data)))
                    '()))))))))


(define* (make-work-pool jobs key: (thread-count 1))
  (define job-pool jobs)
  (define pool-mutex (make-mutex))

  (define results (list))
  (define results-mutex (make-mutex))

  (define (pool-worker)
    (call/ec
     (lambda (return)
       (while #t
         (let ((job (with-mutex pool-mutex
                      (if (null? job-pool)
                          (return #f)
                          (let ((job (car job-pool)))
                            (set! job-pool (cdr job-pool))
                            job)))))
           (catch #t
             (lambda ()
               (let ((result ((job-thunk job))))
                 (with-mutex results-mutex
                   (set! results (cons result results)))))
             (lambda args
               (println (format #f "Job [~a] failed: ~s"
                                (jobname job)
                                ;; TODO better error formatting
                                args)))))))))

  (define threads (map (lambda (i) (make-thread pool-worker (format #f "Worker ~a" i)))
                       (iota thread-count)))

  (for-each thread-start! threads)

  (begin-thread (for-each thread-join! threads)
                results))


;;; Checks if the given argument is truthy.
;;; Raises 'wrong-number-of-args otherwise.
(define-syntax-rule (assert-arg name)
  (unless name
    (scm-error 'wrong-number-of-args "run-tests"
               "Missing required argument ~a"
               '(name) #f)))




(define option-spec
  '((help (single-char #\h))
    (verbose (single-char #\v))
    (suite (value #t))
    (file (value #t))
    (list (single-char #\l))
    (nice (value #t))
    (threads (value #t))
    (coverage (value optional))
    ))

(define help "
run-unit-tests [flags ...]

Run calp's unit tests. While running, Ctrl-C prints the current
status of each file's tests.

Flags:
--help|-h
  print this help
--verbose|-v
  Enables verbose output. This can also be done by setting the
  environment variable VERBOSE.
--suite
  Limit execution to a single test suite. Should be given as a
  directory, and that directory should contain a number of test
  files. See files in tests/unit for available suites.
  Mutualy exclusive with --file.
--file
  Only runs tests from the given file.
  Mutually exlusive with --suite.
--list|-l
  Don't run test, but list all files which would have been ran.
--nice
  How much do incrument the nice value
--threads
  How many threads to spawn for running tests.
--coverage [output-filename]
  Generate code coverage data, this causes the tests to be quite
  a bit slower.
")


(define (main args)
  (define options (getopt-long args option-spec))

  (when (option-ref options 'help #f)
    (display help)
    (exit 0))

  (when (option-ref options 'verbose #f)
    (setenv "VERBOSE" "1"))

  (define test-files
    (cond ((option-ref options 'suite #f)
           => (lambda (suite)
                (glob (path-append suite "*"))))
          ((option-ref options 'file #f) => list)
          (else (glob "tests/unit/**/*.scm"))))

  (nice (string->number (option-ref options 'nice "10")))

  (test-runner-factory construct-test-runner)

  (define coverage
    (let ((cov (option-ref options 'coverage #f)))
      (cond ((string? cov) cov)
            (cov "coverage.info")
            (else #f))))

  ((@ (hnh util exceptions) warnings-are-errors) #t)

  (if (option-ref options 'list #f)
      (format #t "Gathered the following tests:~%~y~%" test-files)
      (let ((results (thread-join!
                      (make-work-pool
                       (prepare-jobs test-files coverage?: coverage)
                       thread-count: (string->number
                                      (option-ref options 'threads "1"))))))
        (define merged-coverages
          (map (lambda (group) (reduce merge-coverage #f (cdr group)))
               (group-by filename (concatenate results))))

        (unless (null? merged-coverages)
          (with-output-to-file coverage
            (lambda ()
              (display "TN:") (newline)
              (for-each output-coverage merged-coverages)))))))

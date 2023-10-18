#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

make --silent calp
make --silent unit-test-deps

root=$(dirname "$(realpath "$0")")
eval "$(env __PRINT_ENVIRONMENT=1 "${root}/calp")"

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
             (srfi srfi-111)
             ((rnrs io ports) :select (get-bytevector-all))
             ((hnh util) :select (-> ->> for group-by))
             (hnh util path)
             (hnh util type)
             (hnh util object)
             (hnh util coverage)
             (hnh util atomic)
             (hnh util atomic-stack)
             (hnh util atomic-queue)
             (hnh test testrunner)
             (hnh test util)
             ((hnh util io) :select (displayln))
             (crypto)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 getopt-long)
             (ice-9 control)
             (ice-9 format))



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

(define (test-runner-name runner)
 (cond ((test-runner-test-name runner)
        (negate string-null?) => identity)
       ((test-result-ref runner 'expected-value)
        => (lambda (p) (with-output-to-string
                    (lambda ()
                      ;; (display (bold "[SOURCE]: "))
                      ;; (truncated-print p width: 60)
                      (write p)
                      ))))))


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



(define runners (atomic-stack))

(sigaction SIGINT
  (lambda _
    (display
     (string-join (map test-runner-test-description (stack->list runners))
                  "\n"))))

(define-type (job)
  (jobname type: string?)
  (job-thunk type: thunk?))

(define* (maybe-with-code-coverage thunk key: (coverage? #f))
  (if coverage?
      (with-code-coverage thunk)
      (values #f (thunk))))


(define (format-test-runner-crash-message args)
  (with-output-to-string
    (lambda ()
      (display
       (red
        (format
         #f
         "Test crashed unexpectedly, after: ~s~%"
         (test-runner-test-name/description (test-runner-current)))))
      (display (yellow "  All further tests in file will be skipped"))
      (newline)
      (format #t "  In file ~s, sightly after line ~a~%"
              (test-result-ref (test-runner-current) 'source-file)
              (test-result-ref (test-runner-current) 'source-line))

      (apply (case-lambda ((err proc fmt args data)
                           (if proc
                               (format #t "  ~a thrown in ~a. ~?~%"
                                       err proc fmt args)
                               (format #t "~a thrown. ~?~%"
                                       err fmt args)))
                          (args (format #t "  ~s~%" args)))
             args))))


(define* (prepare-jobs test-files error-queue key: (coverage? #f))
  (for entry in test-files
       (job
        jobname: (basename entry)
        job-thunk:
        (lambda ()
          (parameterize ((current-filename (basename entry)))
            (test-begin entry)
            (push! (test-runner-get) runners)
            (define-values (coverage module-names)
              (catch #t
                (lambda ()
                  (maybe-with-code-coverage
                   (lambda () (load entry))
                   coverage?: coverage?))
                (lambda args
                  (enqueue! (format-test-runner-crash-message args)
                            error-queue)
                  (values #f '()))))
            (define tested-files (map module->source-file module-names))
            (test-end)
            (if coverage
                (let ((lcov-data
                       (call-with-output-string
                         (lambda (port) (coverage-data->lcov coverage port)))))
                  (filter (lambda (coverage)
                            (member (filename coverage)
                                    tested-files))
                          (parse-coverage lcov-data)))
                '()))))))


(define* (make-work-pool jobs key: (thread-count 1))
  (define job-pool (atomic-stack))
  (define results (atomic-stack))
  (for-each (lambda (job) (push! job job-pool)) jobs)

  (define (pool-worker)
    (call/ec
     (lambda (return)
       (while #t
         (let ((job (pop! job-pool)))
           (unless job (return #f))
           (catch #t
             (lambda () (push! ((job-thunk job)) results))
             (lambda args
               ;; TODO indicate FATAL ERROR
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
    (coverage-supplement (value #t))
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
--suite path
  Limit execution to a single test suite. Should be given as a
  directory, and that directory should contain a number of test
  files. See files in tests/unit for available suites.
  Mutualy exclusive with --file.

  Example: tests/unit/general
--file filename
  Only runs tests from the given file.
  Mutually exlusive with --suite.
--list|-l
  Don't run test, but list all files which would have been ran.
--nice increment
  How much do incrument the nice value
--threads count
  How many threads to spawn for running tests.
--coverage [output-filename]
  Generate code coverage data, this causes the tests to be quite
  a bit slower.
--coverage-supplement supplement-file

")



(define (read-coverage-info port)
  (map (lambda (entry)
         (call-with-values (lambda () (apply values entry))
           (lambda (filename checksum . lines)
             (cons checksum
                   (coverage-info
                    filename: (realpath filename)
                    lines: (map (lambda (l) (cons l 1)) lines))))))
       (read port)))


;;; TODO replace this with native Guile variant
(define (all-files-under-dir dir)
  (let ((pipe (open-pipe*
               OPEN_READ
               "find" dir
               "-type" "f"
               "-name" "*.scm"
               "-exec" "realpath" "--zero" "{}" ";")))
    (let loop ((done '()))
      (let ((line (read-delimited "\0" pipe)))
        (if (eof-object? line)
            (begin
              (close-pipe pipe)
              done)
            (loop (cons line done)))))))


(define (main args)
  (define options (getopt-long args option-spec))

  (when (option-ref options 'help #f)
    (display help)
    (exit 0))

  (when (option-ref options 'verbose (getenv "VERBOSE"))
    (verbose? #t))

  (define test-files
    (cond ((option-ref options 'suite #f)
           => (lambda (suite)
                (glob (path-append suite "*.scm"))))
          ((option-ref options 'file #f) => list)
          (else (glob "tests/unit/**/*.scm"))))

  (nice (string->number (option-ref options 'nice "10")))

  (define error-queue (atomic-queue))

  (test-runner-factory (construct-test-runner
                        print
                        error-queue))

  (define coverage
    (let ((cov (option-ref options 'coverage #f)))
      (cond ((string? cov) cov)
            (cov "coverage.info")
            (else #f))))

  ;; Guile's coverage system sometimes miss some definitions.
  ;; Add these here so the output gets green.
  ;; However, always start by attempting to add more tests to fill
  ;; in the coverage.
  (define-values (extra-coverage outdated-extra)
    (cond ((option-ref options 'coverage-supplement #f)
           => (lambda (supplement-file)
                (partition
                 (lambda (pair)
                   (let ((checksum coverage (car+cdr pair)))
                     (-> (call-with-input-file (filename coverage)
                           get-bytevector-all)
                         sha256
                         checksum->string
                         (string=? checksum))))
                 (call-with-input-file supplement-file read-coverage-info))))
          (else (values '() '()))))

  (unless (null? outdated-extra)
    (format #t "The following files have changed since their coverage")
    (format #t "exceptions were written. Please review:~%")
    (for-each (compose displayln yellow) outdated-extra))

  ((@ (hnh util exceptions) warnings-are-errors) #t)

  (if (option-ref options 'list #f)
      (format #t "Gathered the following tests:~%~y~%" test-files)
      (begin
        (test-begin "Universe")
        (let ((results (thread-join!
                        (make-work-pool
                         (prepare-jobs test-files error-queue coverage?: coverage)
                         thread-count: (string->number
                                        (option-ref options 'threads "1"))))))

          (test-end "Universe")
          (define merged-coverages
            (map (lambda (group) (reduce merge-coverage #f (cdr group)))
                 (group-by filename (concatenate (cons (map cdr extra-coverage)
                                                       (stack->list results))))))


          (define uncovered-files
            (lset-difference! string=?
             (all-files-under-dir "module/")
             (map filename merged-coverages)))

          (unless (null? merged-coverages)
            (with-output-to-file coverage
              (lambda ()
                (display "TN:") (newline)
                (for-each output-coverage merged-coverages)
                (for-each output-coverage
                          (map (lambda (filename)
                                 (coverage-info filename: filename
                                                lines: '((2 . 0))
                                                total-lines: 1
                                                hit-lines: 0))
                               uncovered-files)))))

          (format #t "~%== Gathered errors ==~%")
          (let loop ()
            (cond ((dequeue! error-queue)
                   => (lambda (entry)
                        (display entry)
                        (newline)
                        (loop)))))))))

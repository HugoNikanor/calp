#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

make -j --silent unit-test-deps

root=$(dirname "$(realpath "$0")")
eval "$(env __PRINT_ENVIRONMENT=1 "${root}/calp")"

exec "$GUILE" --debug --no-auto-compile -e main -s "$0" "$@"
!#

;;; Commentary:
;;; Code:

(use-modules (system vm coverage)
             (srfi srfi-1)
             (srfi srfi-18)
             (srfi srfi-64)
             (srfi srfi-71)
             ((rnrs io ports) :select (get-bytevector-all get-u8))
             (calp translation)
             ((hnh util) :select (-> ->> for group-by))
             (hnh util path)
             (hnh util type)
             (hnh util object)
             (hnh util options)
             (hnh util coverage)
             (hnh util atomic)
             (hnh util atomic-stack)
             (hnh util atomic-queue)
             (hnh util destructure)
             (hnh test testrunner)
             (hnh test util)
             ((hnh util io) :select (displayln))
             ((hnh module-introspection all-modules)
              :select (all-files-under-directory))
             (crypto)
             (datetime)
             (ice-9 rdelim)
             (ice-9 getopt-long)
             (ice-9 control)
             (ice-9 format))



(define exemption-rxs
  (list
   ;; All definitions are ignored, since they quite often are missed
   ;; (and don't really "run")
   "\\(define(-(\\w|-)+)?[*]?\\s"
   "read-hash-extend"
   ;; Syntax-rules are usually covered, but not always
   "syntax-rules"
   "; NOCOV[^\"]*$"))



(define-syntax-rule (begin-thread forms ...)
  (let ((thread (make-thread (lambda () forms ...))))
    (thread-start! thread)
    thread))

(define (line-count port)
  (let loop ((count 0)
             (chr (get-u8 port)))
    (if (eof-object? chr)
        count
        (loop (+ count (if (= chr 10)
                           1 0))
              (get-u8 port)))))



(define print display)
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
               (values #f '()))
             (lambda _
               ;; TODO make backtrace configurable
               ;; TODO backtrace should be placed AFTER the error
               (enqueue! (with-output-to-string (lambda () (backtrace))) error-queue)
               )))
         (catch 'wrong-type-arg
           (lambda () (typecheck module-names (list-of (list-of symbol?))))
           (lambda (_ __ fmt args ___)
             (enqueue! (red (format #f "File doesn't end with a module list: ~s" entry))
                       error-queue)))
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
             '()))))





(define option-spec
  `((help (single-char #\h) (description ,(G_ "Prints this help.")))
    (verbose (single-char #\v)
             (description ,(G_ "
Enables verbose output. This can also be done by setting the
environment variable VERBOSE.")))
    (suite (value #t)
           (description (*TOP*
                         ,(G_ "
Limit execution to a single test suite. Should be given as a
directory, and that directory should contain a number of test
files. See files in tests/unit for available suites.
Can be given multiple times, and alongside --file")
                         (br) (br)
                         ,(G_ "Example: tests/unit/general"))))
    (file (value #t)
          (description ,(G_ "
Only runs tests from the given file.
Can be given multiple times, and used alongside --suite
")))
    (list (single-char #\l)
          (description
           ,(G_ "Don't run test, but list all files which would have been ran.")))
    (nice (value #t)
          (description ,(G_ "How much do incrument the nice value")))
    (coverage (value output-filename)
              (description ,(G_ "
Generate code coverage data, this causes the tests to be quite
a bit slower.
")))
    (coverage-supplement (value #t))
    (skip (value #t)
          ;; TODO what exactly is format.
          ;; e.g. how does the test-skip procedure work?
          (description ,(G_ "Test to skip. Can be given multiple times.")))
    ))

(define (read-coverage-info port)
  (map (lambda (entry)
         (call-with-values (lambda () (apply values entry))
           (lambda (filename checksum . lines)
             (cons checksum
                   (coverage-info
                    filename: (realpath filename)
                    lines: (map (lambda (l) (cons l 1)) lines))))))
       (read port)))

;; Return the line numbers of all lines which
;; contain an instance of the given regex
(define (matching-lines rx port)
  (let loop ((lino 1) (hit '()))
    (let ((line (read-line port)))
      (if (eof-object? line)
          hit
          (if (regexp-exec rx line)
              (loop (1+ lino) (cons lino hit))
              (loop (1+ lino) hit))))))


(define (main args)
  (define options (getopt-long args (getopt-opt option-spec)))

  (when (option-ref options 'help #f)
    (format #t (G_ "~a [flags ...]~%") (car (command-line)))
    (display (G_ "
Run calp's unit tests. While running, Ctrl-C prints the current
status of each file's tests.
"))
    (newline)
    (print-arg-help option-spec (current-output-port))
    (exit 0))

  (when (option-ref options 'verbose (getenv "VERBOSE"))
    (verbose? #t))

  (define test-files
    (let ((selected
           (let loop ((options args))
             (cond ((null? options) '())
                   ((string=? "--file" (car options))
                    (cons (cadr options)
                          (loop (cddr options))))
                   ((string=? "--suite" (car options))
                    (append (all-files-under-directory (cadr options) ".scm")
                            (loop (cddr options))))
                   (else (loop (cdr options)))))))
      (if (null? selected)
          (all-files-under-directory "tests/unit" ".scm")
          selected)))


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
    (for-each (lambda (line)
                (displayln
                 (yellow
                  (format #f "~a, expected SHA256: ~a"
                          (filename (cdr line))
                          (car line)))))
              outdated-extra))

  (define coverage-exemptions
    (let ((rx (make-regexp (string-join exemption-rxs "|" 'infix))))
      (map (lambda (filename)
             (coverage-info
              filename: (realpath filename)
              lines: (map (lambda (l) (cons l 1))
                          (call-with-input-file filename
                            (lambda (port) (matching-lines rx port))))))
           (all-files-under-directory "module" ".scm"))))

  ((@ (hnh util exceptions) warnings-are-errors) #t)

  (if (option-ref options 'list #f)
      (format #t "Gathered the following tests:~%~y~%" test-files)
      (begin
        (test-begin "Universe")

        (let loop ((args args))
          (destructure args
            ('() 'done)
            ((cons* "--skip" skip rest)
             (test-skip skip)
             (loop rest))
            ((cons _ rest)
             (loop rest))))


        (let ((results (prepare-jobs test-files error-queue coverage?: coverage)))
          (test-end "Universe")

          (define expected-files (concatenate (cons (map cdr extra-coverage)
                                                    results)))

          (define uncovered-files
            (lset-difference! string=?
             (map realpath (all-files-under-directory "module" ".scm"))
             (map filename expected-files)))

          (define merged-coverages
            (map (lambda (group) (reduce merge-coverage #f (cdr group)))
                 (group-by filename
                           (append
                            expected-files
                            ;; Remove totally uncovered files from the
                            ;; excepmption list. Otherwise they would
                            ;; (accidentally) get a really high coverage
                            ;; percentage, instead of 0%.

                            ;; TODO possibly also remove vendored files,
                            ;; Locking them to 1/1 lines covered (line 2).
                            (remove (lambda (entry) (member (filename entry)
                                                       uncovered-files))
                                    coverage-exemptions)))))


          (format (current-error-port)
                  "~a uncovered files~%~a covered files~%"
                  (length uncovered-files) (length merged-coverages))

          (unless (null? merged-coverages)
            (with-output-to-file (datetime->string (current-datetime) coverage)
              (lambda ()
                (display "TN:") (newline)
                (for-each output-coverage merged-coverages)
                (for-each output-coverage
                          (map (lambda (filename)
                                 (let ((lines (call-with-input-file filename line-count)))
                                   (coverage-info filename: filename
                                                  lines: (map (lambda (x) (cons x 0))
                                                              (iota lines 1))
                                                  total-lines: lines
                                                  hit-lines: 0)))
                               uncovered-files)))))

          (format #t "~%== Gathered errors ==~%")
          (let loop ()
            (cond ((dequeue! error-queue)
                   => (lambda (entry)
                        (display entry)
                        (newline)
                        (loop)))))))))

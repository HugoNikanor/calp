(define-module (hnh test testrunner)
  :use-module (srfi srfi-64)
  :use-module (hnh test util)
  :use-module (hnh util type)
  :use-module (hnh util atomic-queue)
  :use-module (ice-9 pretty-print)
  :use-module (ice-9 format)
  :use-module (ice-9 curried-definitions)
  :export (verbose? construct-test-runner
                    test-runner-test-name/description))

(define verbose? (make-parameter #f))

(define (pp form indent prefix-1)
  (let ((prefix (make-string (+ (string-length indent)
                                (string-length prefix-1))
                             #\space)))
    (string-replace-head
     (with-output-to-string
       (lambda () (pretty-print
              form
              display?: #f
              per-line-prefix: prefix
              width: (- 79 (string-length indent)))))
     (string-append indent prefix-1))))

;;; Return a "name" for the test.
;;; If the test was explicitly named, than that name will be used.
;;; Otherwise a string describing the expected value will be returned.
(define (test-runner-test-name/description runner)
  (cond ((test-runner-test-name runner)
         (negate string-null?) => identity)
        ((test-result-ref runner 'expected-value)
         => (lambda (p) (with-output-to-string
                     (lambda ()
                       (display (bold "[SOURCE]: "))
                       (truncated-print p width: 60)))))))


(define (test-runner-describe-error runner depth)
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
                     (display
                      (string-join
                       (map (lambda (line) (string-append indent "|" line))
                            (string-split (diff expected actual) #\newline))
                       "\n" 'suffix)))))))))

  (format #t "~aNear ~a:~a~%"
          (make-indent (1+ depth))
          (test-result-ref runner 'source-file)
          (test-result-ref runner 'source-line))
  (pretty-print (test-result-ref runner 'source-form)
                (current-output-port)
                per-line-prefix: (string-append (make-indent (1+ depth)) "> ")))

(define ((construct-test-runner print err-queue))
  (typecheck err-queue atomic-queue?)
  (define runner (test-runner-null))
  ;; TODO wouldn't `depth` need to be atomic to work?
  (define depth 0)

  (test-runner-on-test-begin! runner
    (lambda (runner)
      ;; This should be thread local, TODO test that
      (test-runner-aux-value! runner (transform-time-of-day (gettimeofday)))))

  (test-runner-on-test-end! runner
    (lambda (runner)
      (when (verbose?) (display (make-indent depth)))
      (display
       (case (test-result-kind runner)
         ((pass)  (green "X"))
         ((fail)  (red "E"))
         ((xpass) (red "X"))
         ((xfail) (yellow "E"))
         ((skip)  (yellow "-"))))

      (when (or (verbose?) #;(eq? 'fail (test-result-kind)))
        (format #t " ~a~%" (test-runner-test-name/description runner)))

      (case (test-result-kind)
        ((fail)
         (enqueue!
          (with-output-to-string
            (lambda ()
              (display
               (yellow (format #f "Test failed: ~a~%"
                               (test-runner-test-name/description runner))))
              (display
               (yellow
                (format #f "  Path: ~s~%"
                        (cdr (test-runner-group-path runner)))))
              (test-runner-describe-error runner 0)))
          err-queue))
        ((xpass)
         (enqueue!
          (string-append
           (yellow (format #f "Test unexpectedly passed: ~a~%"
                           (test-runner-test-name/description runner)))
           (format #f "  Path: ~s~%"
                   (cdr (test-runner-group-path runner))))
          err-queue)))

      (let ((start (test-runner-aux-value runner))
            (end (transform-time-of-day (gettimeofday))))
        (when (< (µs 1) (- end start))
          (enqueue!
           (format #f "~%Slow test: ~s, took ~a~%"
                   (test-runner-test-name/description runner)
                   (exact->inexact (/ (- end start) (µs 1))))
           err-queue)))

      ))

  ;; on start of group
  (test-runner-on-group-begin! runner
    ;; count is number of #f
    (lambda (runner name count)
      (cond ((<= depth 0)
              (format #t "~a ~a ~a~%"
                      (make-string 10 #\=)
                      name
                      (make-string 10 #\=)))
            ((verbose?)
             (format #t "~a~a~%" (make-string (* depth 2) #\space) name))
            ((= depth 1)
             (format #t "~a: " name))
            (else #f))
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


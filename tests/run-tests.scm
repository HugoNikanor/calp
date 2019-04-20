#!/usr/bin/guile \
-s
!#

(eval-when (compile load)
 (define here (dirname (current-filename))))

(add-to-load-path (format #f "~a/module"
                          (dirname here)))


(use-modules (ice-9 ftw)
             (ice-9 sandbox))

(define files
  (scandir here
           (lambda (name)
             (and (< 2 (string-length name))
                  (not (string=? name (basename (current-filename))))
                  (string=? "scm" (string-take-right name 3))))))

(setenv "TESTPATH"
        (format #f "~a/testdata" (dirname here)))

(use-modules (srfi srfi-64))

;; Load tests

(add-to-load-path here)

(test-begin "tests")
(for-each load-from-path files)
(test-end "tests")

(add-to-load-path "../module")

(use-modules (ice-9 documentation)
             (ice-9 format)
             (srfi srfi-1)
             (hnh util path)
             (datetime)
             )

(print-set! quote-keywordish-symbols #f)

(define (module-definition name old-sandbox-definition)
 `(define-module (test ,name)
    :use-module (srfi srfi-64)
    :use-module (srfi srfi-88)
    ,@(concatenate
       (map (lambda (def) `(:use-module (,(car def) :select ,(cdr def))))
            old-sandbox-definition))))

(define (read-multiple port)
  (let loop ((done '()))
    (let ((sexp (read port)))
      (if (eof-object? sexp)
          (reverse done)
          (loop (cons sexp done))))))

(define files
 '("annoying-events.scm"
   "base64.scm"
   "cpp.scm"
   "datetime-compare.scm"
   "datetime.scm"
   "datetime-util.scm"
   "let-env.scm"
   "let.scm"
   "param.scm"
   "recurrence-advanced.scm"
   "recurrence-simple.scm"
   "rrule-serialization.scm"
   "server.scm"
   "srfi-41-util.scm"
   "termios.scm"
   "tz.scm"
   "util.scm"
   "vcomponent-control.scm"
   "vcomponent-datetime.scm"
   "vcomponent-formats-common-types.scm"
   "vcomponent.scm"
   "web-server.scm"
   "xcal.scm"
   "xml-namespace.scm"
   ))

(for-each (lambda (file)
            (format #t "~a~%" file)
            (call-with-output-file (path-append "test" (basename file))
              (lambda (p)
                (define commentary (file-commentary file))
                (unless (string-null? commentary)
                  (format p ";;; Commentary:~%")
                  (for-each (lambda (line) (format p ";; ~a~%" line))
                            (string-split commentary #\newline))
                  (format p ";;; Code:~%~%"))
                (let ((forms (call-with-input-file file read-multiple)))
                  (format p "~y~%" (module-definition (string->symbol (string-drop-right file 4))
                                                      (car forms)))
                  (format p "~{~y~%~}~%" (cdr forms))))))
          files)


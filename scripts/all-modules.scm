(define-module (all-modules)
  :use-module (ice-9 regex)
  :use-module (srfi srfi-1)
  :use-module (ice-9 ftw)
  :use-module (ice-9 match)
  :export (all-modules-under-directory))

(define (all-modules-under-directory dir)
  "Returns two values, all scm files in dir, and all top
level modules in those files"

  (define re (make-regexp "\\.scm$"))

  (define files '())

  (ftw dir (lambda (filename statinfo flag)
             (cond ((and (eq? flag 'regular)
                         (regexp-exec re filename))
                    => (lambda (m)
                         (set! files (cons filename files))
                         #t
                         ))
                   (else #t))))


  (values files
          (filter identity
                  (map (lambda (file)
                         (match (call-with-input-file file read)
                           (('define-module (module ...) _ ...)
                            module)
                           (_ #f)))
                       files))))

(define-module (all-modules)
  :use-module (ice-9 regex)
  :use-module (srfi srfi-1)
  :use-module (ice-9 ftw)
  :use-module (ice-9 match)
  :export (all-files-and-modules-under-directory all-modules-under-directory))

(define (all-files-and-modules-under-directory dir)
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


  (map (lambda (file)
         (list file
               (match (call-with-input-file file read)
                 (('define-module (module ...) _ ...)
                  module)
                 (_ #f))))
       files))

(define (all-modules-under-directory dir)
  "Returns two values, all scm files in dir, and all top
level modules in those files"

  (define pairs (all-files-and-modules-under-directory dir))
  (values
   (map car pairs)
   (filter identity (map cadr pairs))))

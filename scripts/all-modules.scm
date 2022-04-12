(define-module (all-modules)
  :use-module (ice-9 regex)
  :use-module (srfi srfi-1)
  :use-module (ice-9 ftw)
  :use-module (ice-9 match)
  :use-module (hnh util path)
  :use-module (module-introspection)
  :export (all-files-and-modules-under-directory
           all-modules-under-directory
           fs-find-base fs-find))

(define (fs-find dir)
  (define files '())
  (ftw dir (lambda args (set! files (cons args files)) #t))
  files)

;; (define (fs-find proc dir)
;;   (filter proc (fs-find-base dir)))

(define (all-files-and-modules-under-directory dir)
  (define re (make-regexp "\\.scm$"))

  (define files
    (map car
         (filter (match-lambda ((filename _ 'regular)
                                (and (regexp-exec re filename)
                                     (not (file-hidden? filename))))
                               (_ #f))
                 (fs-find dir))))

  (map (lambda (file)
         (list file
               (call-with-input-file file
                 (compose find-module-declaration get-forms))))
       files))

(define (all-modules-under-directory dir)
  "Returns two values, all scm files in dir, and all top
level modules in those files"

  (define pairs (all-files-and-modules-under-directory dir))
  (values
   (map car pairs)
   (filter identity (map cadr pairs))))

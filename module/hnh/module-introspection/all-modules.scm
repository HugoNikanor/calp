(define-module (hnh module-introspection all-modules)
  :use-module (srfi srfi-1)
  :use-module (ice-9 ftw)
  :use-module (ice-9 match)
  :use-module (hnh util path)
  :use-module (hnh module-introspection)
  :export (all-files-and-modules-under-directory
           all-files-under-directory
           all-modules-under-directory
           fs-find
           module-file-mapping
           ))

(define (fs-find dir)
  (define files '())
  (ftw dir (lambda args (set! files (cons args files)) #t))
  files)

(define (string-ends-with? string tail)
  (and (>= (string-length string)
          (string-length tail))
       (string=? tail
                 (substring string
                            (- (string-length string)
                               (string-length tail))))))

(define* (all-files-under-directory dir extension)
  (map car
       (filter (match-lambda ((filename _ 'regular)
                              (and (string-ends-with? filename extension)
                                   (not (file-hidden? filename))))
                             (_ #f))
               (fs-find dir))))

(define (all-files-and-modules-under-directory dir)
  (map (lambda (file)
         (list file
               (call-with-input-file file
                 (compose find-module-declaration get-forms))))
       (all-files-under-directory dir ".scm")))

(define (all-modules-under-directory dir)
  "Returns two values, all scm files in dir, and all top
level modules in those files"

  (define pairs (all-files-and-modules-under-directory dir))
  (values
   (map car pairs)
   (filter identity (map cadr pairs))))

;; Returns an association list from module names the modules
;; containing filename
(define (module-file-mapping dir)
  (filter
   car
   (map (lambda (pair) (cons (cadr pair) (car pair)))
        (all-files-and-modules-under-directory dir))))

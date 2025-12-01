(define-module (calp entry-points get-config)
  :use-module (hnh util)
  :use-module (ice-9 ftw)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)

  :use-module (hnh module-introspection all-modules)
  :use-module (hnh module-introspection)
  :use-module ((hnh util io)
               :select (read-all ensure-newline))

  :use-module ((calp translation)
               :select (translate))

  :export (main))


(define (main args)
  (format #t ";;;~%")
  (format #t ";;; Found configurable options in the program~%")
  (format #t ";;;~%")

  ;; TODO split this into separate read and write stages
  ;; TODO Add extra output formats
  ;; - Texinfo
  ;; - actual configuration files
  (for (filename module-name)
    in (all-files-and-modules-under-directory "module")
    (define forms (call-with-input-file filename (lambda (p) (read-all read p))))
    (define configurations
      (filter (lambda (form)
                (and (list? form) (not (null? form))
                     (eq? 'define-config (car form))))
              forms))
    (unless (null? configurations)
      (newline)
      (format #t "[~{~a~^ ~}]~%" module-name)
      (for-each (match-lambda
                  (('define-config name default kvs ...)
                   (cond ((memv description: kvs)
                          => (match-lambda
                               ((description: (_ desc) rest ...)
                                (format #t ";; ~a~%"
                                        (gettext desc "calp")))
                               ((description: desc rest ...)
                                (format #t ";; ~a~%" desc)))))
                   (format #t "~a = ~s~%"
                           name default)))
                configurations)))

  (newline))

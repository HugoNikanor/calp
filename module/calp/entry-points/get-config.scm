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
               :select (G_ translate))

  :export (%summary main))

(define %summary
  (G_ "Gathers configurable items from the source code."))

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
                  (('define-config name default-value kvs ...)
                   (cond ((memv description: kvs)
                          => (match-lambda
                               ((description: (_ desc) rest ...)
                                (format #t ";; ~a~%"
                                        (gettext desc "calp")))
                               ((description: desc rest ...)
                                (format #t ";; ~a~%" desc)))))
                   (define real-value
                     ((module-ref (resolve-interface module-name)
                                  name)))
                   ;; TODO define-config should include a new field
                   ;; #:value-pretty-print (or similar)
                   ;; which takes the actual value, and displays it
                   ;; in a way suitable for output here
                   (cond ((equal? default-value real-value)
                          (format #t "~a = ~s~%" name default-value))
                         (else
                          (format #t ";; ~a = ~s~%" name default-value)
                          (format #t "~a = ~s~%" name real-value)))))
                configurations)))

  (newline))

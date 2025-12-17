(define-module (calp entry-points get-config)
  :use-module (hnh util)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (hnh util serialize)
  :use-module ((hnh util io)
               :select (read-all ensure-newline))
  :use-module (ice-9 ftw)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)

  :use-module (hnh module-introspection all-modules)
  :use-module (hnh module-introspection)

  :use-module ((web uri) :select (uri->string))
  :use-module ((vcomponent data-stores common)
               :select (calendar-data-store? store-uri))

  :use-module ((calp translation)
               :select (G_ translate))

  :export (%summary main))

(define %summary
  (G_ "Gathers configurable items from the source code."))

(define-type (configuration)
  (module type: (list-of symbol?))
  (name type: symbol?)
  (description type: (or false? string?))
  default
  )

(define (find-configurations file-list)
  (concatenate
   (for (filename module-name) in file-list
        (filter-map (match-lambda
                      (('define-config name default kvs ...)
                       (configuration module: module-name
                                      name: name
                                      default: default
                                      description:
                                      (cond ((memv description: kvs)
                                             => (match-lambda
                                                  ((description: (_ desc) _ ...)
                                                   (gettext desc "calp"))
                                                  ((description: desc _ ...)
                                                   desc)))
                                            (else #f))))
                      (_ #f))
                    (call-with-input-file filename (lambda (p) (read-all read p)))))))

;; TODO define-config should include a new field
;; #:value-pretty-print (or similar)
;; which takes the actual value, and displays it
;; in a way suitable for output here

;; TODO Add extra output formats
;; - Texinfo
;; - actual configuration files

(define (main args)
  (define configuration-items
    (find-configurations (all-files-and-modules-under-directory "module")))

  (format #t ";;;~%")
  (format #t ";;; Found configurable options in the program~%")
  (format #t ";;;~%")

  (with-serializers
   ((calendar-data-store? (compose uri->string store-uri))
    (boolean? (lambda (b) (if b 'true 'false))))
   (for (module-name . configuration-options) in (group-by module configuration-items)
        (newline)
        (format #t "[~{~a~^ ~}]~%" module-name)
        (for config in configuration-options
             (awhen (description config)
                    (format #t ";; ~a~%" it))
             (define real-value
               ((module-ref (resolve-interface module-name)
                            (name config))))

             (cond ((equal? (default config) real-value)
                    (format #t "~a = ~s~%"
                            (name config)
                            (serialize (default config))))
                   (else
                    (format #t ";~a = ~s~%" (name config) (default config))
                    (cond ((expand-validator real-value (list-of pair?))
                           (for-each (lambda (pair)
                                       (format #t "~a[~a] = ~s~%"
                                               (name config)
                                               (car pair)
                                               (serialize (cdr pair))))
                                     real-value)
                           )
                          (else
                           (format #t "~a = ~s~%" (name config)
                                   (serialize real-value)))))))))

  (newline))

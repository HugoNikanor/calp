(define-module (calp entry-points store)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util options)
  :use-module (hnh util)
  :use-module (calp translation)
  :use-module (ice-9 getopt-long)
  :export (%summary main))

(define-public %category 'application)

(define %summary
  "")

(define opt-spec
  `((help (single-char #\h)
          (description ,(G_ "Print this help")))))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)
                            stop-at-first-non-option: #t))

  (when (option-ref opts 'help #f)
    (format #t "TODO help goes here~%")
    (throw 'return))

  (let* ((remaining-options* (option-ref opts '() '()))
         (remaining-options
          (if (null? remaining-options*) '("list") remaining-options*))
         (name (string->symbol (car remaining-options))))
    ((module-ref (resolve-interface `(calp entry-points store ,name))
                 'main)
     remaining-options)))

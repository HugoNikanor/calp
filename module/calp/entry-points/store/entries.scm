(define-module (calp entry-points store entries)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (ice-9 getopt-long)
  :use-module (vcomponent data-stores common)
  :use-module (calp translation)
  :export (%summary main))

(define %summary "")

(define option-spec
  '())

(define (main args)
  (define opts (getopt-long args (getopt-opt option-spec)))

  (for store-name in (option-ref opts '() '())
       (cond ((assoc-ref ((@ (vcomponent config) data-stores))
                         store-name)
              => (lambda (store)
                   (for href in (list-entries/shallow store)
                        (format #t "~a/~a~%" store-name href))))
             (else
              (format (current-error-port)
                      (G_ "No such store: ~a~%")
                      store-name)
              ;; TODO error code
              (throw 'return)))))

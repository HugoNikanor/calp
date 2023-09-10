(define-module (hnh util exceptions)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  :use-module (ice-9 format)

  :use-module ((system vm frame)
               :select (frame-bindings binding-ref))

  :export (warning-handler
           warnings-are-errors
           warning
           fatal
           filter-stack
           ))


(define warning-handler
  (make-parameter
   (lambda (fmt . args)
     (format #f "WARNING: ~?~%" fmt args))))

(define warnings-are-errors
  (make-parameter #f))

;; forwards return from warning-hander. By default returns an unspecified value,
;; but instances are free to provide a proper return value and use it.
(define (warning fmt . args)
  (display (apply (warning-handler) fmt (or args '()))
           (current-error-port))
  (when (warnings-are-errors)
    (throw 'warning fmt args)))

(define (fatal fmt . args)
  (display (format #f "FATAL: ~?~%" fmt (or args '()))
           (current-error-port))
  (raise SIGINT))


(define (filter-stack pred? stk)
  (concatenate
   (for i in (iota (stack-length stk))
        (filter pred? (map binding-ref (frame-bindings (stack-ref stk i)))))))

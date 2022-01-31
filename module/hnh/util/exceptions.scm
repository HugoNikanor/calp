(define-module (hnh util exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (hnh util)
  #:use-module (calp util config)
  #:use-module (ice-9 format)

  #:use-module ((system vm frame)
                :select (frame-bindings binding-ref))

  #:export (assert))


(define-public warning-handler
  (make-parameter
   (lambda (fmt . args)
     (format #f "WARNING: ~?~%" fmt args))))

(define-public warnings-are-errors
  (make-parameter #f))

(define-config warnings-are-errors #f
  description: "Crash on warnings."
  post: warnings-are-errors)

;; forwards return from warning-hander. By default returns an unspecified value,
;; but instances are free to provide a proper return value and use it.
(define-public (warning fmt . args)
  (display (apply (warning-handler) fmt (or args '()))
           (current-error-port))
  (when (warnings-are-errors)
    (throw 'warning fmt args)))

(define-public (fatal fmt . args)
  (display (format #f "FATAL: ~?~%" fmt (or args '()))
           (current-error-port))
  (raise 2)
  )

(define (prettify-tree tree)
  (cond [(pair? tree) (cons (prettify-tree (car tree))
                            (prettify-tree (cdr tree)))]
        [(and (procedure? tree) (procedure-name tree))
         => identity]
        [else tree]))


(define-macro (assert form)
  `(unless ,form
     (throw 'assertion-error "Assertion failed. ~a expected, ~a got"
            (quote ,form)
            ((@@ (calp util exceptions) prettify-tree) (list ,form)))))


(define-public (filter-stack pred? stk)
  (concatenate
   (for i in (iota (stack-length stk))
        (filter pred? (map binding-ref (frame-bindings (stack-ref stk i)))))))

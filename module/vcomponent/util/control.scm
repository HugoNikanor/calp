(define-module (vcomponent util control)
  :use-module ((vcomponent) :select (prop))
  :use-module ((srfi srfi-1) :select (fold))
  :export (with-replaced-properties))

(define-syntax with-replaced-properties
  (syntax-rules ()
    [(_ (component (key val) ...)
        body ...)

     (let ((component
            (fold (lambda (pair component)
                    (prop component (car pair) (cdr pair)))
                  component
                  (list (cons (quote key) val) ...))))
       body ...)]))


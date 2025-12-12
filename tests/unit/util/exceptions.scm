(define-module (test exceptions)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util exceptions))

(test-group "Warnings"
 (parameterize ((warning-handler (lambda (fmt . args)
                                   (test-equal "Test: ~a" fmt)
                                   (test-equal '(1) args)))
                (warnings-are-errors #f))
   (warning "Test: ~a" 1))

 (parameterize ((warning-handler (lambda (fmt . args)
                                   (test-equal '(2) args)))
                (warnings-are-errors #t))
   (test-error 'warning
     (warning "~a happened" 2))))

(test-error "Unreachable"
  'unreachable (unreachable "procedure" "fmt" (list)))

'((hnh util exceptions))

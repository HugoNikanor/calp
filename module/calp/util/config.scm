;;; Commentary:

;; Configuration system.

;;; Code:

(define-module (calp util config)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 curried-definitions) ; for ensure
  :use-module (calp translation)
  :export (define-config)
)

(define (fix-keywords args)
  (map (lambda (arg)
         (if (keyword? (syntax->datum arg))
             (syntax->datum arg)
             arg))
       args))

(define %configuration-error
  (_ "Pre-property failed when setting ~s to ~s"))

(define-syntax (define-config stx)
  (syntax-case stx ()
    ((_ name default kw ...)
     (let ((pre  (cond ((memv pre:  (fix-keywords #'(kw ...))) => cadr) (else #f)))
           (post (cond ((memv post: (fix-keywords #'(kw ...))) => cadr) (else #f))))
       #`(begin
           (define-once name
            (make-parameter
             default
             #,@(cond ((and pre post)
                       #`((lambda (new-value)
                            (cond ((#,pre new-value)
                                   => (lambda (translated)
                                        (#,post translated)
                                        translated))
                                  (else
                                   (scm-error 'configuration-error
                                              "set-config!"
                                              %configuration-error
                                              (list (quote name) new-value)))))))
                      (pre
                       #`((lambda (new-value)
                            (or (#,pre new-value)
                                (scm-error 'configuration-error
                                           "set-config!"
                                           %configuration-error
                                           (list (quote name) new-value))))))
                      (post
                       #`((lambda (new-value)
                            (#,post new-value)
                            new-value))
                       )
                      (else #'()))))
           (export name))))))



(define-public ((ensure predicate) value)
  (if (predicate value)
      value #f))

(define-module (hnh util env)
  :export (let-env with-working-directory))

(define-syntax let-env
  (syntax-rules ()
    [(_ ((name value) ...)
        body ...)

     (let ((env-pairs #f))
       (dynamic-wind
         (lambda ()
           (set! env-pairs
             (map (lambda (n new-value)
                    (list n new-value (getenv n)))
                  (list (symbol->string (quote name)) ...)
                  (list value ...)))
           (for-each (lambda (pair)
                       (if (cadr pair)
                           (setenv (car pair)
                                   (cadr pair))
                           (unsetenv (car pair))))
                     env-pairs))
         (lambda () body ...)
         (lambda ()
           (for-each (lambda (pair) (setenv (car pair) (caddr pair)))
                     env-pairs))))]))


;; TODO this probably isn't threadsafe... pthreads(7) notes
;; that chdir is shared between all threads.
(define-syntax-rule (with-working-directory directory thunk)
  (let ((old-cwd #f))
   (dynamic-wind
     (lambda ()
       (set! old-cwd (getcwd))
       (chdir directory))
     thunk
     (lambda () (chdir old-cwd)))))


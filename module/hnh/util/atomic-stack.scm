(define-module (hnh util atomic-stack)
  :use-module (srfi srfi-18)            ; Threading
  :use-module (rnrs records syntactic)
  :use-module (rnrs exceptions)
  :use-module (hnh util atomic)
  :use-module ((hnh util type) :select (typecheck))
  :use-module ((hnh util) :select (begin1))
  :export (atomic-stack
           atomic-stack?
           stack-peek stack->list
           push! pop!))

(define-record-type (stack %atomic-stack atomic-stack?)
  (fields (mutable contents)
          mutex)
  (sealed #t)
  (opaque #t))

(define (atomic-stack)
  (%atomic-stack '() (make-mutex)))

(define (stack->list stack)
  (stack-contents stack))

(define (push! value stack)
  (typecheck stack atomic-stack?)
  (with-mutex (stack-mutex stack)
    (stack-contents-set!
     stack
     (cons value (stack-contents stack)))))

(define (stack-peek stack)
  (typecheck stack atomic-stack?)
  (car (stack-contents stack)))

(define (pop! stack)
  (typecheck stack atomic-stack?)
  (with-mutex (stack-mutex stack)
    (guard (_ (else #f))
      (begin1 (stack-peek stack)
              (stack-contents-set!
               stack (cdr (stack-contents stack)))))))


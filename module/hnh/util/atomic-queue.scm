(define-module (hnh util atomic-queue)
  :use-module (srfi srfi-18)            ; Threading
  :use-module (rnrs records syntactic)
  :use-module (rnrs exceptions)
  ;; :use-module (rnrs mutable-pair)
  :use-module (hnh util atomic)
  :use-module (hnh util type)
  :use-module ((hnh util) :select (begin1))
  :export (atomic-queue
           atomic-queue?
           queue-peek queue->list
           enqueue! dequeue!))



;;; Items are added at the back, and poped from the front

(define-record-type (queue %atomic-queue atomic-queue?)
  (fields front
          (mutable back)
          front-mutex back-mutex)
  (sealed #t)
  (opaque #t))

(define (atomic-queue)
  (let ((p (list 'FRONT)))
    (%atomic-queue p p (make-mutex) (make-mutex))))

(define (enqueue! value q)
  (typecheck q atomic-queue?)
  (with-mutex (queue-back-mutex q)
    (set-cdr! (queue-back q) (list value))
    (queue-back-set! q (cdr (queue-back q)))))

(define (queue-peek q)
  (typecheck q atomic-queue?)
  (cadr (queue-front q)))

(define (dequeue! q)
  (typecheck q atomic-queue?)
  (with-mutex (queue-front-mutex q)
    (guard (_ (else #f))
      (begin1 (queue-peek q)
              (set-cdr! (queue-front q)
                        (cddr (queue-front q)))))))

(define (queue->list q)
  (typecheck q atomic-queue?)
  (cdr (queue-front q)))

(define-module (test atomic-queue)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util atomic-queue))

;;; TODO multithreaded tests

(define q (atomic-queue))

(enqueue! 1 q)
(enqueue! 2 q)
(enqueue! 3 q)

(test-equal 1 (dequeue! q))

(enqueue! 4 q)

(test-equal 2 (dequeue! q))
(test-equal 3 (dequeue! q))
(test-equal 4 (dequeue! q))
(test-equal #f (dequeue! q))
(test-equal #f (dequeue! q))

(test-group "Errors are capturable"
 (catch #t
   (lambda ()
     (queue-peek q)
     (test-assert "Should never be reached" #f))
   (lambda _ (test-assert #t "Error correctly captured"))))


'((hnh util atomic-queue))

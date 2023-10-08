(define-module (test atomic-stack)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util atomic-stack))

(define stack (atomic-stack))

(test-equal "Fresh stacks are empty"
  '() (stack->list stack))

(push! 1 stack)
(push! 2 stack)
(push! 3 stack)

(test-equal "Stack contents when content"
  '(3 2 1) (stack->list stack))

(test-equal "Poped correctly 3" 3 (pop! stack))
(push! 4 stack)
(test-equal "Poped correctly 4" 4 (pop! stack))
(test-equal "Poped correctly 2" 2 (pop! stack))
(test-equal "Poped correctly 1" 1 (pop! stack))
(test-equal "Poped correctly #f" #f (pop! stack))

'((hnh util atomic-stack))

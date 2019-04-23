(define-module (util tree)
  #:use-module (srfi srfi-1)
  #:use-module (util)
  #:export (make-tree left-subtree
                      right-subtree
                      length-of-longst-branch
                      tree-map))

;; Constructs a binary tree where each node's children is partitioned
;; into a left and right branch using @var{pred?}.
;; Has thee form @var{(node left-subtree right-subtree)}. A leaf has
;; both it's children equal to @var{null}.
(define (make-tree pred? lst)
  (if (null? lst) '()
      (let* ((head tail (partition (lambda (el) (pred? (car lst) el))
                                   (cdr lst))))
        (list (car lst)
              (make-tree pred? head)
              (make-tree pred? tail)))))

(define (left-subtree tree)
  (list-ref tree 1))

(define (right-subtree tree)
  (list-ref tree 2))

;; Length includes current node, so the length of a leaf is 1.
(define (length-of-longst-branch tree)
  (if (null? tree)
      0 (1+ (max (length-of-longst-branch (left-subtree tree))
                 (length-of-longst-branch (right-subtree tree))))))

(define (tree-map proc tree)
  (if (null? tree) '()
      (list (proc (car tree))
            (tree-map proc (left-subtree tree))
            (tree-map proc (right-subtree tree)))))

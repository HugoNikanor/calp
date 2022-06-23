(define-module (hnh util tree)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (hnh util)
  :export (make-tree left-subtree
                     right-subtree
                     tree-node
                     length-of-longst-branch
                     tree-map))

;; Constructs a binary tree where each node's children is partitioned
;; into a left and right branch using @var{pred?}.
;; Has thee form @var{(node left-subtree right-subtree)}. A leaf has
;; both it's children equal to @var{null}.
(define (make-tree pred? lst)
  (unless (null? lst)
    (let ((head tail (partition (lambda (el) (pred? (car lst) el))
                                (cdr lst))))
      (list (car lst)
            (make-tree pred? head)
            (make-tree pred? tail)))))

(define (tree-node tree)
  (car tree))

(define (left-subtree tree)
  (list-ref tree 1))

(define (right-subtree tree)
  (list-ref tree 2))

;; Length includes current node, so the length of a leaf is 1.
(define (length-of-longst-branch tree)
  (if (null? tree)
      ;; Having the @var{1+} outside the @var{max} also works,
      ;; but leads to events overlapping many other to be thinner.
      ;; Having it inside makes all events as evenly wide as possible.
      0 (max (1+ (length-of-longst-branch (left-subtree tree)))
             (length-of-longst-branch (right-subtree tree)))))

(define (tree-map proc tree)
  (unless (null? tree)
    (list (proc (car tree))
          (tree-map proc (left-subtree tree))
          (tree-map proc (right-subtree tree)))))

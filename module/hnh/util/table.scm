;;; Commentary:
;;; An immutable key-value table.
;;;
;;; Currently implemented as a simple binary search tree,
;;; this may however change at any time.
;;; Code:

(define-module (hnh util table)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util type)
  :use-module (ice-9 curried-definitions)
  :export ((make-tree . table)
           (tree-get . table-get)
           (tree-put . table-put)
           (tree-remove . table-remove)
           (tree->list . table->list)
           (tree? . table?)
           (tree-terminal? . table-empty?)
           (tree-focus . table-focus)
           (tree-equal? . table-equal?)
           (serialize-tree . serialize-table)
           (alist->tree . alist->table)))

(define (symbol<? . args)
  (apply string<? (map symbol->string args)))

(define-syntax-rule (symbol< args ...)
  (string< (symbol->string args) ...))

(define (serialize-tree t)
  `(-> (table)
       ,@(fold (lambda (p done)
                 (cons `(table-put ,(serialize (car p)) ,(serialize (cdr p)))
                       done))
               '()
               (tree->list t))))

(define-type (tree-node
              serializer: serialize-tree
              constructor: (lambda (constructor type-check)
                             (lambda* (key: key value type
                                            (left (tree-terminal type: type))
                                            (right (tree-terminal type: type)))
                               ;; (format #t "args: ~s~%" )
                               (type-check key value left right type)
                               (when type
                                 (typecheck value type))
                               (constructor key value left right type))))
  (key type: symbol?)
  value
  (left type: tree? default: (tree-terminal))
  (right type: tree? default: (tree-terminal))
  (node-type type: (or procedure? false?) default: #f keyword: type))

;; Type tagged null
(define-type (tree-terminal serializer: (lambda _ '(table)))
  (terminal-type type: (or procedure? false?) default: #f keyword: type))

;; Wrapped for better error messages
(define* (make-tree optional: type) (tree-terminal type: type))

(define (tree? x)
  (or (tree-node? x)
      (tree-terminal? x)))

(define* (type x optional: (v (nothing)))
  (typecheck v optional?)
  (define accessor
   (cond ((tree-node? x) node-type)
         ((tree-terminal? x) terminal-type)))
  (if (just? v)
      (accessor x (from-just v))
      (accessor x)))

;; Lens for focusing a specific entry in a table.
(define (((tree-focus k) tree) op)
  (cond ((tree-terminal? tree)
         (cond ((op (nothing)) just?
                => (lambda (v) (tree-node key: k value: (from-just v)
                                     type: (type tree))))
               (else (tree-terminal type: (type tree)))))
        ((eq? k (key tree))
         (cond ((op (just (value tree)))
                just? => (lambda (v) (value tree (from-just v))))
               (else (type (merge-trees (left tree) (right tree))
                           (just (type tree))))))
        (else
         (modify tree (lens-compose (if (symbol<? k (key tree))
                                        left* right*)
                                    (tree-focus k))
                 op))))

(define (tree-equal? a b)
  (or (and (tree-terminal? a) (tree-terminal? b))
      (tree-equal? (left a) (left b))
      (tree-equal? (right a) (right b))))

(define (tree-put tree k v)
  (set tree (tree-focus k) (just v)))

(define* (tree-get tree k optional: default)
  (unjust (get tree (tree-focus k)) default))

(define (tree-remove tree k)
  (set tree (tree-focus k) (nothing)))

;;; Merge two trees.
;;; Note that this discards type information
(define (merge-trees a b)
  ;; TODO write a better version of this
  ;; Possibly one which re-balances the trees
  (fold (lambda (pair tree)
          (tree-put tree (car pair) (cdr pair)))
        a
        (tree->list b)))

;; in-order traversal
(define (tree->list tree)
  (if (tree-terminal? tree)
      '()
      (append (tree->list (left tree))
              (list (cons (key tree) (value tree)))
              (tree->list (right tree)))))

;; undefined order, probably pre-order
(define (tree-map f tree)
  (if (tree-terminal? tree)
      '()
      (tree-node key:   (key tree)
                 value: (f (key tree) (value tree))
                 left:  (tree-map f (left tree))
                 right: (tree-map f (right tree)))))

;; pre-order
(define (tree-fold f init tree)
  (if (tree-terminal? tree)
      init
      (let ((a (f (key tree) (value tree) init)))
        (let ((b (tree-fold f a (left tree))))
          (tree-fold f b (right tree))))))

(define (alist->tree alist)
  (fold (lambda (kv tree) (tree-put tree (car kv) (cdr kv)))
        (tree-terminal)
        alist))



(define (make-indent depth) (make-string (* 2 depth) #\space))

(define* (print-tree tree optional: (depth 0))
  (unless (tree-terminal? tree)
    (format #t "~a- ~s: ~s~%" (make-indent depth) (key tree) (value tree))
    (print-tree (left tree) (1+ depth))
    (print-tree (right tree) (1+ depth))))

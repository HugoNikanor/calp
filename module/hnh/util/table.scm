(define-module (hnh util table)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :export ((make-tree . table)
           (tree-get . table-get)
           (tree-put . table-put)
           (tree? . table?)
           (alist->tree . alist->table)))

(define (symbol<? . args)
  (apply string<? (map symbol->string args)))

(define-syntax-rule (symbol< args ...)
  (string< (symbol->string args) ...))

(define-type (tree-node)
  (key type: symbol?)
  value
  (left type: tree? default: (tree-terminal))
  (right type: tree? default: (tree-terminal)))

;; Type tagged null
(define-type (tree-terminal))

;; Wrapped for better error messages
(define (make-tree) (tree-terminal))

(define (tree? x)
  (or (tree-node? x)
      (tree-terminal? x)))

(define (tree-put tree k v)
  (cond ((tree-terminal? tree) (tree-node key: k value: v))
        ((eq? k (key tree)) (value tree v))
        (else
         (modify tree (if (symbol<? k (key tree)) left right)
                 tree-put k v))))

(define (tree-get tree k)
  (cond ((tree-terminal? tree) (throw 'out-of-range))
        ((eq? k (key tree)) (value tree))
        ((symbol<? k (key tree))
         (tree-get (left tree) k))
        (else
         (tree-get (right tree) k))))

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
      (tree-node (key tree)
                 (f (key tree) (value tree))
                 (tree-map f (left tree))
                 (tree-map f (right tree)))))

;; pre-order
(define (tree-fold f init tree)
  (if (tree-terminal? tree)
      init
      (let ((a (f (key tree) (value tree) init)))
        (let ((b (tree-fold f a (left tree))))
          (tree-fold f b (right tree))))))

(define (alist->tree alist)
  (fold (lambda (kv tree) (apply tree-put tree kv))
        (tree-terminal)
        alist))



(define (make-indent depth) (make-string (* 2 depth) #\space))

(define* (print-tree tree optional: (depth 0))
  (unless (tree-terminal? tree)
    (format #t "~a- ~s: ~s~%" (make-indent depth) (key tree) (value tree))
    (print-tree (left tree) (1+ depth))
    (print-tree (right tree) (1+ depth))))

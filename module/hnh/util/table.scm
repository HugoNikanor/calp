;;; Commentary:
;;; An immutable key-value table.
;;;
;;; Currently implemented as a simple binary search tree,
;;; this may however change at any time.
;;; Code:

(define-module (hnh util table)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util type)
  :use-module (hnh util named-type)
  :use-module (ice-9 curried-definitions)
  :export ((make-tree . table)
           (tree-type . table-type)
           (tree-of . table-of)
           (tree-get . table-get)
           (tree-put . table-put)
           (tree-remove . table-remove)
           (tree-pop . table-pop)
           (tree->list . table->list)
           (tree? . table?)
           (tree-terminal? . table-empty?)
           (tree-focus . table-focus)
           (tree-equal? . table-equal?)
           (tree-diff . table-diff)
           (serialize-tree . serialize-table)
           (alist->tree . alist->table)))

(define (symbol<? . args)
  (apply string<? (map symbol->string args)))

(define-syntax-rule (symbol< args ...)
  (string< (symbol->string args) ...))

(define (serialize-tree t)
  `(-> (table ,@(if (tree-type t) (list (serialize (tree-type t))) '()))
       ,@(map (lambda (p) `(table-put ,(serialize (car p)) ,(serialize (cdr p))))
              (tree->list t))))

(define-type (tree-node
              serializer: serialize-tree
              constructor: (lambda (constructor type-check)
                             (lambda* (key: key value type
                                            (left (tree-terminal type: type))
                                            (right (tree-terminal type: type)))
                               (type-check key value left right type)
                               (when type
                                 (typecheck value ((named-type-type type))
                                            "tree-node" (named-type-name type)))
                               (constructor key value left right type))))
  (key type: symbol?)
  value
  (left type: tree? default: (tree-terminal))
  (right type: tree? default: (tree-terminal))
  (node-type type: (or false? named-type?) default: #f keyword: type))

;; Tree node without content. Holds a reference to the type, which it
;; will share when added to
(define-type (tree-terminal
              serializer:
              (lambda (t) `(table ,@(if (tree-type t) (list (serialize (tree-type t))) '()))))
  (terminal-type type: (or false? named-type?) default: #f keyword: type))

;; Wrapped for better error messages
(define* (make-tree optional: type) (tree-terminal type: type))

(define (tree? x)
  (or (tree-node? x)
      (tree-terminal? x)))



(define* (tree-type x optional: (v (nothing)))
  (typecheck v optional?)
  (define accessor
    (cond ((tree-node? x) node-type)
          ((tree-terminal? x) terminal-type)))
  (if (just? v)
      (accessor x (from-just v))
      (accessor x)))


(define-syntax-rule (tree-of var inner)
  (and (tree? var)
       (tree-type var)
       ;; NOTE this unfortunately breaks type aliasing.
       ;; Figure out how to expand
       (equal? (quote inner)
               (named-type-name (tree-type var)))))


;; Lens for focusing a specific entry in a table.
(define (((tree-focus k) tree) op)
  (cond ((tree-terminal? tree)
         (let ((ret (op (nothing))))
           (cond ((just? ret)
                  (tree-node key: k value: (from-just ret)
                             type: (tree-type tree)))
                 ((nothing? ret) (tree-terminal type: (tree-type tree)))
                 (else (scm-error 'misc-error "tree-focus"
                                  "Non-wrapped value returned to tree-focus: ~s"
                                  (list ret)
                                  #f)))))
        ((eq? k (key tree))
         (let ((ret (op (just (value tree)))))
           (cond ((just? ret) (value tree (from-just ret)))
                 ((nothing? ret) (tree-type (merge-trees (left tree) (right tree))
                                            (just (tree-type tree))))
                 (else (scm-error 'misc-error "tree-focus"
                                  "Non-wrapped value returned to tree-focus: ~s"
                                  (list ret)
                                  #f)))))
        (else
         (modify tree (lens-compose (if (symbol<? k (key tree))
                                        left* right*)
                                    (tree-focus k))
                 op))))

(define* (tree-equal? a b optional: (comperator equal?))
  (or (and (tree-terminal? a) (tree-terminal? b))
      (let loop ((as (tree->list a))
                 (b b))
        (if (null? as)
            (tree-terminal? b)
            (let ((v rest (tree-pop b (caar as))))
              (and (not (nothing? v))
                   (comperator (from-just v) (cdar as))
                   (loop (cdr as) rest)))))))

;;; Returns a list of diff-objects, where diff objects on the form
;;; '(absent ,table ,key) ; where table is the symbol 'a or 'b
;;; '(diff ,key ,a-value ,b-value)
(define* (tree-diff a b optional: (comperator equal?))
  (if (and (tree-terminal? a) (tree-terminal? b))
      '()
      (let loop ((as (tree->list a))
                 (b b))
        (cond ((and (null? as) (tree-terminal? b)) '())
              ((null? as)
               (map (lambda (p) `(absent a ,(car p)))
                    (tree->list b)))
              (else
               (let ((v rest (tree-pop b (caar as))))
                 (cond ((nothing? v)
                        (cons `(absent b ,(caar as))
                              (loop (cdr as) rest)))
                       ((comperator (from-just v) (cdar as))
                        (loop (cdr as) rest))
                       (else
                        (cons (list 'diff (caar as) (cdar as) (from-just v))
                              (loop (cdr as) rest))))))))))

(define (tree-put tree k v)
  (set tree (tree-focus k) (just v)))

;;; TODO rename to `tree-ref`?
(define* (tree-get tree k optional: default)
  (unjust (get tree (tree-focus k)) default))

(define (tree-remove tree k)
  (set tree (tree-focus k) (nothing)))

;;; Returns 2 values
;;; - maybe (the focused value)
;;; - the table without that key
(define (tree-pop tree k)
  (let ((result (nothing)))
    (let ((resulting-tree
           (modify tree (tree-focus k)
                   (lambda (m)
                     (set! result m)
                     (nothing)))))
      (values result resulting-tree))))

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

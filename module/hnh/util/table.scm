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
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util serialize)
  :use-module (hnh util type)
  :use-module (hnh util named-type)
  :use-module (hnh util destructure)
  :use-module (ice-9 curried-definitions)
  :export (table
           table-type
           table-of
           table-get
           table-preview
           table-put
           table-remove
           table-pop
           table-any
           table->list
           table?
           (table-terminal? . table-empty?)
           table-focus
           table-equal?
           table-diff
           serialize-table
           table-filter-map
           table-union
           table-intersection
           table-difference
           alist->table))

(define (symbol<? . args)
  (apply string<? (map symbol->string args)))

(define-syntax-rule (symbol< args ...)
  (string< (symbol->string args) ...))

(define (serialize-table t)
  `(-> (table ,@(if (table-type t) (list (serialize (table-type t))) '()))
       ,@(table->list t (lambda (k v) `(table-put ,(serialize k) ,(serialize v))))))

(define-type (table-node
              serializer: serialize-table
              constructor: (lambda (constructor type-check)
                             (lambda* (key: key value type
                                            (left (table-terminal type: type))
                                            (right (table-terminal type: type)))
                               (type-check key value left right type)
                               (when type
                                 (typecheck value ((named-type-type type))
                                            "table-node" (named-type-name type)))
                               (constructor key value left right type))))
  (key type: symbol?)
  value
  (left type: table? default: (table-terminal))
  (right type: table? default: (table-terminal))
  (node-type type: (or false? named-type?) default: #f keyword: type))

;; Table node without content. Holds a reference to the type, which it
;; will share when added to
(define-type (table-terminal
              serializer:
              (lambda (t) `(table ,@(if (table-type t) (list (serialize (table-type t))) '()))))
  (terminal-type type: (or false? named-type?) default: #f keyword: type))

;; Wrapped to handle type as optional argument, while allowing
;; both inner and leaf nodes to be constructed as (proc type: type).
(define* (table optional: type) (table-terminal type: type))

(define (table? x)
  (or (table-node? x)
      (table-terminal? x)))



(define* (table-type x optional: (v (nothing)))
  (typecheck v optional?)
  (define accessor
    (cond ((table-node? x) node-type)
          ((table-terminal? x) terminal-type)))
  (if (just? v)
      (accessor x (from-just v))
      (accessor x)))


(define-syntax-rule (table-of var inner)
  (and (table? var)
       (table-type var)
       ;; NOTE this unfortunately breaks type aliasing.
       ;; Figure out how to expand
       (equal? (quote inner)
               (named-type-name (table-type var)))))


;; Lens for focusing a specific entry in a table.
(define (((table-focus k) table) op)
  (cond ((table-terminal? table)
         (let ((ret (op (nothing))))
           (cond ((just? ret)
                  (table-node key: k value: (from-just ret)
                              type: (table-type table)))
                 ((nothing? ret) (table-terminal type: (table-type table)))
                 (else (scm-error 'misc-error "table-focus"
                                  "Non-wrapped value returned to table-focus: ~s"
                                  (list ret)
                                  #f)))))
        ((eq? k (key table))
         (let ((ret (op (just (value table)))))
           (cond ((just? ret) (value table (from-just ret)))
                 ((nothing? ret) (table-type (merge-tables (left table) (right table))
                                             (just (table-type table))))
                 (else (scm-error 'misc-error "table-focus"
                                  "Non-wrapped value returned to table-focus: ~s"
                                  (list ret)
                                  #f)))))
        (else
         (modify table (lens-compose (if (symbol<? k (key table))
                                         left* right*)
                                     (table-focus k))
                 op))))

(define* (table-equal? a b optional: (comperator equal?))
  (or (and (table-terminal? a) (table-terminal? b))
      (let loop ((as (table->list a))
                 (b b))
        (if (null? as)
            (table-terminal? b)
            (let ((v rest (table-pop b (caar as))))
              (and (not (nothing? v))
                   (comperator (from-just v) (cdar as))
                   (loop (cdr as) rest)))))))

;;; Returns a list of diff-objects, where diff objects on the form
;;; '(absent ,table ,key) ; where table is the symbol 'a or 'b
;;; '(diff ,key ,a-value ,b-value)
(define* (table-diff a b optional: (comperator equal?))
  (if (and (table-terminal? a) (table-terminal? b))
      '()
      (let loop ((as (table->list a))
                 (b b))
        (cond ((and (null? as) (table-terminal? b)) '())
              ((null? as)
               (map (lambda (p) `(absent a ,(car p)))
                    (table->list b)))
              (else
               (let ((v rest (table-pop b (caar as))))
                 (cond ((nothing? v)
                        (cons `(absent b ,(caar as))
                              (loop (cdr as) rest)))
                       ((comperator (from-just v) (cdar as))
                        (loop (cdr as) rest))
                       (else
                        (cons (list 'diff (caar as) (cdar as) (from-just v))
                              (loop (cdr as) rest))))))))))


;;; TODO rename to `table-ref`?
(define* (table-get table k optional: default)
  (unjust (get table (table-focus k)) default))

(define (table-preview table k)
  (get table (table-focus k)))

(define (table-remove table k)
  (set table (table-focus k) (nothing)))

;;; Returns 2 values
;;; - maybe (the focused value)
;;; - the table without that key
(define (table-pop table k)
  (let ((result (nothing)))
    (let ((resulting-table
           (modify table (table-focus k)
                   (lambda (m)
                     (set! result m)
                     (nothing)))))
      (values result resulting-table))))

;;; Return an arbitrary value from the table, alongside the table without that value
;;; table-any :: table -> (optional x), table
(define (table-any table)
  (if (table-terminal? table)
      (values (nothing) table)
      (table-pop table (key table))))

(define (table-put table k v)
  (set table (table-focus k) (just v)))


;;; Merge two tables.
;;; Note that this discards type information
(define (merge-tables a b)
  ;; TODO write a better version of this
  ;; Possibly one which re-balances the tables
  (fold (lambda (pair table)
          (table-put table (car pair) (cdr pair)))
        a
        (table->list b)))

;; in-order traversal
(define* (table->list table optional: (proc cons))
  (if (table-terminal? table)
      '()
      (append (table->list (left table) proc)
              (list (proc (key table) (value table)))
              (table->list (right table) proc))))

;; undefined order, probably pre-order
(define (table-filter-map f table)
  (let recurse ((table table))
    (if (table-terminal? table)
        table
        (destructure (f (key table) (value table))
          ((just v)
           (-> table
               (set value* v)
               (modify left*  recurse)
               (modify right* recurse)))
          ((nothing)
           (merge-tables (recurse (left table))
                         (recurse (right table))))))))

(define (alist->table alist)
  (fold (lambda (kv table) (table-put table (car kv) (cdr kv)))
        (table-terminal)
        alist))



;; right-biased
;; reversed operand order, since reduce calls (proc elem prev)
(define (table-union% b a)
  (fold (lambda (pair table)
          (table-put table (car pair) (cdr pair)))
        a
        (table->list b)))

(define (table-union . as)
  (typecheck as (list-of table?))
  (reduce table-union% (table) as))

;; left-biased
(define (table-intersection% a b)
  (table-filter-map
   (lambda (k _) (table-preview b k))
   a))

(define (table-intersection . as)
  (typecheck as (list-of table?))
  (reduce table-intersection% (table) as))

(define (table-difference a b)
  (fold (swap table-remove)
        a (table->list b (lambda (k _) k))))




(define (make-indent depth) (make-string (* 2 depth) #\space))

(define* (print-table table optional: (depth 0))
  (unless (table-terminal? table)
    (format #t "~a- ~s: ~s~%" (make-indent depth) (key table) (value table))
    (print-table (left table) (1+ depth))
    (print-table (right table) (1+ depth))))

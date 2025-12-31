(define-module (test module-introspection dependency-loops)
  :use-module (srfi srfi-64)
  :use-module (hnh module-introspection dependency-loops)
  :use-module (hnh util table)
  :use-module (hnh util set)
  :use-module (hnh util lens))


(define (data->module-resolver data)
  (let ((tab (alist->table
              (map (lambda (p) (modify p cdr* list->set))
                   data))))
    (lambda (k) (table-get tab k (data-set)))))

(define (data->universe data)
  (list->set (map car data)))


;;; Both samples consists of a large graph with cycles,
;;; and a tiny disjoint graph with a cycle.

;;; ╭──→ A ←──╮
;;; │    ↓    │
;;; │    B    │  e ⮂ f
;;; ↑  ↙   ↘  ↑
;;; ╰ C     D ╯
(define sample-1
  '((a b c)
    (b d)
    (c d)
    (d a)

    (e f)
    (f e)
    ))


;;;    A ←──╮
;;;  ↙   ↘  │
;;; B     C │  e ⮂ f
;;;  ↘   ↙  │
;;;    D →──╯
(define sample-2
  '((a b)
    (b c d)
    (c a)
    (d a)

    (e f)
    (f e)
    ))

;;; NOTE that the order of the returned loops isn't stable.
;;; Add sorting once it becomes a problem

(test-equal "Graph 1"
  '((e f)
    (a b d))
  (find-loops
   (data->universe sample-1)
   (data->module-resolver sample-1)))

(test-equal "Graph 2"
  '((e f)
    (a b c)
    (a b d))
  (find-loops
   (data->universe sample-2)
   (data->module-resolver sample-2)))

'((hnh module-introspection dependency-loops))

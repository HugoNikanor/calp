(define-module (test hnh-util-graph)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (->))
  :use-module (hnh util graph)
  )

(define g (make-graph string->symbol
                      string=?))

(test-assert "New graphs are empty" (graph-empty? g))

(let ((g2 (-> g
              (add-node "a" '())
              (add-node "b" '(a)))))
  (test-assert "Changing graph doesn't change source graph"
    (graph-empty? g))

  (test-equal '("a" "b")
   (resolve-dependency-graph g2)))

'((hnh util graph))

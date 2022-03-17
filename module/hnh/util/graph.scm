;;; Commentary:
;; An immutable directed graph.
;; Most operations are O(n), since there is no total
;; order on symbols in scheme.
;;; Code:

(define-module (hnh util graph)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9 gnu))

;; Immutable directed graph
(define-immutable-record-type <graph>
  (make-graph% nodes edges node-key-proc node-equal?)
  graph?
  (nodes graph-nodes)
  (edges graph-edges) ; (list (symb . symb))
  (node-key-proc node-key-proc) ; node → symb
  (node-equal? node-equal?) ; node, node -> symb
  )

(define*-public (make-graph optional:
                            (node-key-proc identity)
                            (node-equal? eq?))
  (make-graph% '() '() node-key-proc node-equal?))

(define*-public (rebuild-graph optional: old-graph
                               (nodes '()) (edges '()))
  (make-graph% nodes edges
               (if old-graph (node-key-proc old-graph) identity)
               (if old-graph (node-equal? old-graph) eq?)))

(define-public (graph-empty? graph)
  (null? (graph-nodes graph)))

;; Add node to graph. Adds directed edges from node to neighbours
;; graph, node, (list node-key) → graph
(define-public (add-node graph node edge-neighbours)
  (rebuild-graph
   graph
   (lset-adjoin (node-equal? graph) (graph-nodes graph)
                node)
   (lset-union equal? (graph-edges graph)
               (map (lambda (o) (cons ((node-key-proc graph) node) o))
                    edge-neighbours))))

;; get node by key
(define-public (get-node graph key)
  (find (lambda (node) (eq? key ((node-key-proc graph) node)))
        (graph-nodes graph)))

;; Remove node by @var{node-equal?}
(define-public (remove-node graph node)
  (rebuild-graph
   graph
   (remove (lambda (other) ((node-equal? graph) node other))
           (graph-nodes graph))
   (let ((key ((node-key-proc graph) node)))
     (remove (lambda (edge) (or (eq? key (car edge))
                           (eq? key (cdr edge))))
             (graph-edges graph)))))

;; NOTE this is O(n^2) (maybe, sort of?)
;; Getting it faster would require building an index, which
;; is hard since there isn't a total order on symbols.
(define-public (find-node-without-dependencies graph)
  (find (lambda (node)
          (let ((key ((node-key-proc graph) node)))
            (not (find (lambda (edge) (eq? key (car edge))) (graph-edges graph)))))
        (graph-nodes graph)))

;; graph → node x graph
(define-public (find-and-remove-node-without-dependencies graph)
  (let ((node (find-node-without-dependencies graph)))
    (unless node
      (scm-error 'graph-error "find-and-remove-node-without-dependencies"
                 "No node without dependencies in graph"
                 #f (list graph)))
    (values node (remove-node graph node))))

;; Assumes that the edges of the graph are dependencies.
;; Returns a list of all nodes so that each node is before its dependants.
;; A missing dependency (and probably a loop) is an error, and currently
;; leads to some weird error messages.
(define-public (resolve-dependency-graph graph)
  (catch 'graph-error
    (lambda ()
      (let loop ((graph graph))
        (if (graph-empty? graph)
            '()
            (let* ((node graph* (find-and-remove-node-without-dependencies graph)))
              (cons node (loop graph*))))))
    (lambda (err caller fmt args data)
      (car graph))))

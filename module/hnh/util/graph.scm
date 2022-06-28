;;; Commentary:
;; An immutable directed graph.
;; Most operations are O(n), since there is no total
;; order on symbols in scheme.
;;; Code:

(define-module (hnh util graph)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-9 gnu)
  :use-module (ice-9 format)
  :export (make-graph
           rebuild-graph
           graph-empty?
           add-node
           get-node
           remove-node
           find-dangling-node
           pop-dangling-node
           resolve-dependency-graph))

;; Immutable directed graph
(define-immutable-record-type <graph>
  (make-graph% nodes edges node-key-proc node-equal?)
  graph?
  (nodes graph-nodes)
  (edges graph-edges) ; (list (symb . symb))
  (node-key-proc node-key-proc) ; node → symb
  (node-equal? node-equal?) ; node, node -> symb
  )

(define* (make-graph optional:
                     (node-key-proc identity)
                     (node-equal? eq?))
  (make-graph% '() '() node-key-proc node-equal?))

(define* (rebuild-graph optional: old-graph
                        (nodes '()) (edges '()))
  (make-graph% nodes edges
               (if old-graph (node-key-proc old-graph) identity)
               (if old-graph (node-equal? old-graph) eq?)))

(define (graph-empty? graph)
  (null? (graph-nodes graph)))

;; Add node to graph. Adds directed edges from node to neighbours
;; graph, node, (list node-key) → graph
(define (add-node graph node edge-neighbours)
  (rebuild-graph
   graph
   (lset-adjoin (node-equal? graph) (graph-nodes graph)
                node)
   (lset-union equal? (graph-edges graph)
               (map (lambda (o) (cons ((node-key-proc graph) node) o))
                    edge-neighbours))))

;; get node by key
(define (get-node graph key)
  (find (lambda (node) (eq? key ((node-key-proc graph) node)))
        (graph-nodes graph)))

;; Remove node by @var{node-equal?}
(define (remove-node graph node)
  (rebuild-graph
   graph
   (remove (lambda (other) ((node-equal? graph) node other))
           (graph-nodes graph))
   (let ((key ((node-key-proc graph) node)))
     (remove (lambda (edge) (or (eq? key (car edge))
                           (eq? key (cdr edge))))
             (graph-edges graph)))))

;; A dangling node is a node which nothing depends on.

;; NOTE this is O(n^2) (maybe, sort of?)
;; Getting it faster would require building an index, which
;; is hard since there isn't a total order on symbols.
(define (find-dangling-node graph)
  (find (lambda (node)
          (let ((key ((node-key-proc graph) node)))
            (not (find (lambda (edge) (eq? key (car edge))) (graph-edges graph)))))
        (graph-nodes graph)))

;; graph → node x graph
(define (pop-dangling-node graph)
  (let ((node (find-dangling-node graph)))
    (unless node
      (scm-error 'graph-error "pop-dangling-node"
                 "No node without dependencies in graph"
                 '() (list graph)))
    (values node (remove-node graph node))))

;; Assumes that the edges of the graph are dependencies.
;; Returns a list of all nodes so that each node is before its dependants.
;; A missing dependency (and probably a loop) is an error, and currently
;; leads to some weird error messages.
(define (resolve-dependency-graph graph)
  (catch 'graph-error
    (lambda ()
      (let loop ((graph graph) (done '()))
        (if (graph-empty? graph)
            (reverse done)
            (let ((node graph* (pop-dangling-node graph)))
              (loop graph* (cons node done))))))
    (lambda (err proc fmt args data)
      (format (current-error-port)
              "~a in ~a: ~?~%"
              err proc fmt args)
      (format (current-error-port)
              "~s~%" (car data))
      )))

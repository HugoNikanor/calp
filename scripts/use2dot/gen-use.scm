#!/usr/bin/guile -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules ((scripts frisk) :select (make-frisker edge-type edge-up
                              edge-down))
             (srfi srfi-1)
             (ice-9 ftw)
             (ice-9 regex)
             (ice-9 match)
             ((graphviz) :prefix gv.)
             )

(define scan (make-frisker `(default-module . (calp main))))

(define re (make-regexp "\\.scm$"))

(define lst '())

(ftw "module" (lambda (filename statinfo flag)
                (cond ((and (eq? flag 'regular)
                            (regexp-exec re filename))
                       => (lambda (m)
                            (set! lst (cons filename lst))
                            #t
                            ))
                      (else #t))))



(define files lst)

(define our-modules
  (filter identity
          (map (lambda (file)
                 (match (call-with-input-file file read)
                        (('define-module (module ...) _ ...)
                         module)
                        (_ #f)))
               files)))

(define graph (gv.digraph "G"))
(gv.setv graph "color" "blue")
(gv.setv graph "compound" "true")
(gv.setv graph "overlap" "prism")
;; (gv.setv graph "bgcolor" "blue")

(define count 0)

(define colors
  '("red" "green" "blue"))

(define rem our-modules)

;; (for-each (lambda (key)
;; 
;;             (define subgraph (gv.graph graph (format #f "cluster_~a" count)))
;; 
;;             (define-values (use rem*) (partition (lambda (mod) (eq? key (car mod))) rem))
;;             (set! rem rem*)
;; 
;;             ;; (gv.setv subgraph "rankdir" "TB")
;;             (gv.setv subgraph "color" (list-ref colors count))
;; 
;;             (for-each (lambda (name)
;;                         (gv.node subgraph (format #f "~a" name)))
;;                       use)
;; 
;;             (set! count (1+ count))
;;             )
;;           '(calp vcomponent))

;; (define subgraph (gv.graph graph (format #f "cluster_~a" count)))
;; 
;; ;; (gv.setv subgraph "rankdir" "TB")
;; (gv.setv subgraph "color" (list-ref colors count))
;; 
;; (for-each (lambda (name)
;;             (gv.node subgraph (format #f "~a" name)))
;;           rem)

(define subgraph (gv.graph graph (format #f "cluster_~a" 0)))

;; (gv.setv subgraph "rankdir" "TB")
(gv.setv subgraph "color" "Red")

(define subgraphs (make-hash-table))

(for-each (lambda (name)
            (let ((g (hashq-ref subgraphs (car name)
                                (gv.graph graph (format #f "cluster_~a" (car name))))))
              (hashq-set! subgraphs (car name) g)

              (let ((node (gv.node g (format #f "~a" name))))
                (gv.setv node "fillcolor" "green")
                (gv.setv node "style" "filled")
                ))
            )
          (remove (lambda (x) (eq? 'calp (car x)))
                  our-modules))


(define calp-base (gv.graph graph "cluster_1"))
(define calpgraphs (make-hash-table))

(for-each (lambda (name)
            (let ((g (hashq-ref calpgraphs (cadr name)
                                (gv.graph 
                                  ;; calp-base 
                                  graph
                                  (format #f "cluster_~a" (cadr name))))))
              (hashq-set! calpgraphs (car name) g)

              (let ((node (gv.node g (format #f "~a" name))))
                (gv.setv node "fillcolor" "green")
                (gv.setv node "style" "filled")
                ))
            )
          (remove (compose null? cdr)
                  (filter (lambda (x) (eq? 'calp (car x)))
                          our-modules)))


(define (remove-edges blacklist edges)
  (remove (lambda (edge)
            (or (member (edge-up edge) blacklist)
                (member (edge-down edge) blacklist)))
          edges))





(for-each (lambda (edge)
            (let ((gv-edge (gv.edge graph
                                 (format #f "~a" (edge-down edge))
                                 (format #f "~a" (edge-up edge))
                                 )))
              (when (and (eq? 'calp (car (edge-up edge)))
                         (not (eq? 'calp (car (edge-down edge)))))
                (gv.setv gv-edge "color" "red"))
              (when (and (memv (car (edge-up edge)) '(vcomponent calp))
                         (not (memv (car (edge-down edge)) '(vcomponent calp ))))
                (gv.setv gv-edge "color" "blue"))
              ))
          (remove-edges '((srfi srfi-1)
                          (srfi srfi-9)
                          (srfi srfi-26)
                          (srfi srfi-41)

                          (ice-9 match)
                          (ice-9 format)

                          (datetime)
                          (vcomponent)
                          (hnh util)
                          )
                        ((scan files) 'edges)))

(gv.layout graph "fdp")
(gv.render graph "pdf" "graph.pdf")


(display "done\n")

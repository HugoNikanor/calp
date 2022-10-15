(define-module (scripts use2dot-all)
  :use-module ((scripts frisk) :select (make-frisker edge-type edge-up
                                                     edge-down))
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)
  :use-module ((graphviz) :prefix gv.)
  :use-module (hnh module-introspection all-modules)
  :use-module (hnh util options)
  :use-module (ice-9 getopt-long)
  :export (main))

(define default-remove
 '((srfi srfi-1)
   (srfi srfi-9)
   (srfi srfi-26)
   (srfi srfi-41)

   (ice-9 match)
   (ice-9 format)))

(define option-spec
  `((engine (value #t)
            (description "Graphviz rendering engine to use. Defaults to FDP"))
    (default-module
      (single-char #\m)
      (value #t)
      (description "Set MOD as the default module, see guild help use2dot for more information. Defaults to (guile-user)"))
    (output
     (single-char #\o)
     (value #t)
     (description "Name of output PDF"))
    (remove
     (value #t)
     (description "Modules to remove from check, usually since to many other modules depend on them."))
    (ignore-default-remove
     (description "Don't ignore the modules which are ignored by default, which are:" (br)
                  ,@(append-map (lambda (item) (list (with-output-to-string (lambda () (display item))) '(br)))
                                default-remove)))))

(define %synopsis "use2dot-all [options] <directory>")
(define %summary "Like use2dot, but for multiple modules")
(define %help (format-arg-help option-spec))

(define (remove-edges blacklist edges)
  (remove (lambda (edge)
            (or (member (edge-up edge) blacklist)
                (member (edge-down edge) blacklist)))
          edges))

(define (main . args)
  (define options (getopt-long (cons "use2dot-all" args)
                               (getopt-opt option-spec)
                               stop-at-first-non-option: #t))
  (define default-module
    (cond ((option-ref options 'default-module #f)
           => (lambda (s) (let ((mod (with-input-from-string s read)))
                       (unless (list? mod)
                         (format (current-error-port)
                                 "Module must be a list: ~s~%" mod)
                         (exit 1)))))
          (else '(guile-user))))
  (define engine (option-ref options 'engine "fdp"))
  (define output-file (option-ref options 'output "graph.pdf"))
  (define custom-remove (cond ((option-ref options 'remove #f)
                               => (lambda (s) (let ((lst (with-input-from-string s read)))
                                           (unless (and (list? lst) (every list? lst))
                                             (format (current-error-port)
                                                     "custom-remove must get a list of lists: ~s~%" lst)
                                             (exit 1))
                                           lst)))
                              (else '())))
  (define to-remove (if (option-ref options 'default-remove #f)
                        custom-remove
                        (append custom-remove default-remove)))
  (define target-directory
    (let ((remaining (option-ref options '() '())))
      (cond ((null? remaining)
             (format (current-error-port) "Target directory required~%")
             (exit 1))
            (else (car remaining)))))

  ;; End of command line parsing

  (define scan (make-frisker `(default-module . ,default-module)))

  (define-values (files our-modules)
    (all-modules-under-directory target-directory))

  (define graph
    (let ((graph (gv.digraph "G")))
      (gv.setv graph "color" "blue")
      (gv.setv graph "compound" "true")
      (gv.setv graph "overlap" "prism")
      ;; (gv.setv graph "bgcolor" "blue")
      graph))

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

  (define subgraph
    (let ((subgraph (gv.graph graph (format #f "cluster_~a" 0))))
      ;; (gv.setv subgraph "rankdir" "TB")
      (gv.setv subgraph "color" "Red")
      subgraph))


  (define subgraphs
    (let ((subgraphs (make-hash-table)))
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
                        our-modules))))

  (define calp-base (gv.graph graph "cluster_1"))
  (define calpgraphs
    (let ((calpgraphs (make-hash-table)))
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
      calpgraphs))


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
            (remove-edges to-remove
                          ((scan files) 'edges)))

  (gv.layout graph engine)
  (gv.render graph "pdf" output-file))

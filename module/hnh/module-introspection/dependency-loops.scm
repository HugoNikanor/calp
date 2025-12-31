(define-module (hnh module-introspection dependency-loops)
  :use-module (hnh util optional)
  :use-module (hnh util destructure)
  :use-module (hnh util set)
  :use-module (hnh util type)
  :use-module (srfi srfi-71)
  :use-module ((sxml fold) :select (fold-values))
  :export (find-loops)
  )

;;; universe: set of all module names we care about module names
;;; module-dependencies: module-name -> (set-of module-name)
;;;   must be defined for all module names
(define (find-loops universe module-dependencies)
  (typecheck universe set?)
  (typecheck module-dependencies procedure?)

  ;; This procedure manually models a state monad, with `unvisited` in
  ;; the state. This code base (at least at the time of writing)
  ;; contains a Haskell inspired state monad in the form of (hnh util
  ;; state-monad). However, that forwards state in a slightly
  ;; different way than expected here.
  ;; Notably, we assume that all procedures work on explicit
  ;; arguments, and never pulls anything from the "hidden" state.

  (let outer ((loops '())
              (unvisited universe))
    (let ((result unvisited
                  ;; (type-alias module-name (list-of symbol?))
                  ;; (procedure
                  ;;  (from (list-of module-name)
                  ;;        (optional-of module-name))
                  ;;        (set-of module-name)
                  ;;        ;; should be list of at least length 2
                  ;;  (to   (list-of (non-empty-list-of module-name))
                  ;;        (set-of module-name)))
                  (let inner ((stk '())
                              (token (nothing))
                              (unvisited unvisited))
                    (destructure token
                      ((and (just v) (member v stk))
                       ;; loop detected
                       (values (list (reverse stk)) unvisited))
                      (_
                       (let ((node* unvisited (set-pop unvisited token)))
                         (destructure node*
                           ((just node)
                            (fold-values
                             (lambda (child old-matches unvisited)
                               (let ((new-matches unvisited
                                                  (inner (cons node stk)
                                                         (just child)
                                                         unvisited)))
                                 (values (append old-matches new-matches)
                                         unvisited)))
                             (set->list
                              (set-intersection
                               universe (module-dependencies node)))
                             '() unvisited))

                           ;; dead end encountered
                           ((nothing) (values '() unvisited)))))))))

      (if (set-empty? unvisited)
          (append result loops)
          (outer (append result loops) unvisited)))))

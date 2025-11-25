(define-module (hnh util type)
  :use-module ((srfi srfi-1) :select (every))
  :export (build-validator-body
           list-of pair-of pair-of* tuple-of
           non-empty-list-of
           false? any-type
           typecheck
           current-procedure-name
           ))

;;; TODO could this be simplified due to how build-validator-body works?
(define-syntax list-of
  (syntax-rules ()
    ((_ variable (rule ...))
     (and (list? variable)
          (every (lambda (x) (build-validator-body x (rule ...))) variable)))
    ((_ variable rule)
     (and (list? variable)
          (every rule variable)))))

(define-syntax-rule (non-empty-list-of v p)
  (build-validator-body v (pair-of p (list-of p))))

(define-syntax-rule (pair-of variable a b)
  (and (pair? variable)
       (build-validator-body (car variable) a)
       (build-validator-body (cdr variable) b)))

(define-syntax pair-of*
  (syntax-rules ()
    ((_ variable a)       (build-validator-body variable a))
    ((_ variable a b ...) (pair-of variable a (pair-of* b ...)))))

(define-syntax-rule (tuple-of variable a ... b)
  (pair-of* variable a ... (pair-of b null?)))

;; DSL for specifying type predicates
;; Basically a procedure body, but the variable to test is implicit.
(define-syntax build-validator-body
  (syntax-rules (and or not)
    ((_ v (and clauses ...))  (and (build-validator-body v clauses) ...))
    ((_ v (or clauses ...))   (or (build-validator-body v clauses) ...))
    ((_ v (not clause))       (not (build-validator-body v clause)))
    ((_ v (proc args ...))    (proc v args ...))
    ((_ v proc)               (proc v))))

(define-syntax-rule (current-procedure-name)
  ;; 1 since make-stack is at top of stack
  (frame-procedure-name (stack-ref (make-stack #t) 1)))

(define-syntax typecheck
  (syntax-rules ()
    ((_ expr type-clause)
     (let ((procedure-name (current-procedure-name)))
       (typecheck expr type-clause procedure-name (quote type-clause))))

    ((_ expr type-clause procedure-name)
     (typecheck expr type-clause procedure-name (quote type-clause)))

    ((_ expr type-clause procedure-name type-source)
     (unless (build-validator-body expr type-clause)
       (scm-error 'wrong-type-arg procedure-name
                  "The expression `~s' doesn't satisfy the type `~s'. Evaluated to ~s"
                  (list (quote expr) type-source expr)
                  #f)))))

;;; For use in typechecks, since
;;;   (or false? integer?)
;;; is much clearer than
;;;   (or not integer?)
(define false? not)

;;; For compound types where any field is ok
(define any-type (const #t))

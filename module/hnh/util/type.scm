(define-module (hnh util type)
  :use-module ((srfi srfi-1) :select (every))
  :export (expand-validator
           list-of pair-of pair-of* tuple-of
           non-empty-list-of
           false? any-type
           typecheck
           current-procedure-name
           ))

(define-syntax list-of
  (syntax-rules ()
    ((_ variable (rule ...))
     (and (list? variable)
          (every (lambda (x) (expand-validator x (rule ...))) variable)))
    ((_ variable rule)
     (and (list? variable)
          (every rule variable)))))

(define-syntax-rule (non-empty-list-of v p)
  (expand-validator v (pair-of p (list-of p))))

(define-syntax-rule (pair-of variable a b)
  (and (pair? variable)
       (expand-validator (car variable) a)
       (expand-validator (cdr variable) b)))

(define-syntax pair-of*
  (syntax-rules ()
    ((_ variable a)       (expand-validator variable a))
    ((_ variable a b ...) (pair-of variable a (pair-of* b ...)))))

(define-syntax-rule (tuple-of variable a ... b)
  (pair-of* variable a ... (pair-of b null?)))

;; DSL for specifying type predicates
;; Basically a procedure body, but the variable to test is implicit.
;;; TODO rename to expand-validator
(define-syntax expand-validator
  (syntax-rules (and or not)
    ((_ v (and clauses ...))  (and (expand-validator v clauses) ...))
    ((_ v (or clauses ...))   (or (expand-validator v clauses) ...))
    ((_ v (not clause))       (not (expand-validator v clause)))
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
     (unless (expand-validator expr type-clause)
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

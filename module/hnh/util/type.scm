(define-module (hnh util type)
  :use-module ((srfi srfi-1) :select (every))
  :export (build-validator-body
           list-of pair-of pair-of* tuple-of
           false?
           typecheck
           current-procedure-name))

(define-syntax list-of
  (syntax-rules ()
    ((_ variable (rule ...))
     (and (list? variable)
          (every (lambda (x) (build-validator-body x (rule ...))) variable)))
    ((_ variable rule)
     (and (list? variable)
          (every rule variable)))))

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
    ((_ variable (and clauses ...))  (and (build-validator-body variable clauses) ...))
    ((_ variable (or clauses ...))   (or (build-validator-body variable clauses) ...))
    ((_ variable (not clause))       (not (build-validator-body variable clause)))
    ((_ variable (proc args ...))    (proc variable args ...))
    ((_ variable proc)               (proc variable))))

(define-syntax-rule (current-procedure-name)
  ;; 1 since make-stack is at top of stack
  (frame-procedure-name (stack-ref (make-stack #t) 1)))

(define-syntax typecheck
  (syntax-rules ()
    ((_ variable type-clause)
     (let ((procedure-name (current-procedure-name)))
       (typecheck variable type-clause procedure-name)))
    ((_ variable type-clause procedure-name)
     (unless (build-validator-body variable type-clause)
       (scm-error 'wrong-type-arg procedure-name
                  "Invalid value for ~s. Expected ~s, got ~s"
                  (list (quote variable) (quote type-clause) variable)
                  #f)))))

;;; For use in typechecks, since
;;;   (or false? integer?)
;;; is much clearer than
;;;   (or not integer?)
(define false? not)

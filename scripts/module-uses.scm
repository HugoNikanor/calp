(define-module (module-uses)
  :use-module (ice-9 match)
  :export (module-uses*))

;;; Commentary:
;;; Static analyze version of guile's built in module-uses.
;;; Will give a less accurate result, but in turn doesn't
;;; require that the target module compiles.
;;; Code:

(define (parse-interface-specification interface-specification)
  (match interface-specification
    ;; matches `((srfi srfi-1) :select (something))
    (((parts ...) args ...)
     parts)
    ;; matches `(srfi srfi-1)
    ((parts ...)
     parts)
    (_ (error "Bad module declaration"))))

;; Finds all define-module forms, and returns what they
;; pull in (including autoloads)
(define (module-declaration-uses forms)
  (match forms
    (('define-module module-name directives ...)
     (let loop ((directives directives))
       (cond ((null? directives) '())
             ((memv (car directives) '(#:use-module #{:use-module}#))
              (cons (parse-interface-specification (cadr directives))
                    (loop (cddr directives))))
             ((memv (car directives) '(#:autoload #{:autoload}#))
              (cons (cadr directives)
                    (loop (cdddr directives))))
             (else (loop (cdr directives))))))
    ((form forms ...)
     (append (module-declaration-uses form)
             (module-declaration-uses forms)))
    (_ '())))

;; find all use-modules forms, and return what they pull in
(define (module-use-module-uses forms)
  (match forms
    (('use-modules modules ...)
     (map parse-interface-specification modules))
    ((form forms ...)
     (append (module-use-module-uses form)
             (module-use-module-uses forms)))
    (_ '())))

;; find all explicit module references (e.g.
;; (@ (module) var) and (@@ (module) private-var)),
;; and return those modules
(define (module-refer-uses forms)
  (match forms
    (((or '@ '@@) module _) (list module))
    ((form forms ...)
     (append (module-refer-uses form)
             (module-refer-uses forms)))
    (_ '())))

;; List of all modules pulled in in any of forms
(define (module-uses* forms)
  (append
   (module-declaration-uses forms)
   (module-use-module-uses  forms)
   (module-refer-uses       forms)))

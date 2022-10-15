(define-module (hnh module-introspection module-uses)
  :use-module (ice-9 match)
  :use-module (hnh util)
  :use-module ((srfi srfi-1) :select (concatenate))
  :use-module ((srfi srfi-88) :select (string->keyword))
  :use-module (rnrs records syntactic)
  :export (module-uses*))

;;; Commentary:
;;; Static analyze version of guile's built in module-uses.
;;; Will give a less accurate result, but in turn doesn't
;;; require that the target module compiles.
;;; Code:

(define-record-type (module make-module% module?)
  (fields name select hide prefix renamer version))

(define* (make-module name key:
                      (select #f)
                      (hide '())
                      (prefix #f)
                      (renamer #f)
                      (version #f))
  (make-module% name select hide prefix renamer version))

(define (module->list module)
  (append
   (list (module-name module))
   (awhen (module-select module)  `(#:select ,it))
   (awhen (module-hide module)    `(#:hide ,it))
   (awhen (module-prefix module)  `(#:prefix ,it))
   (awhen (module-renamer module) `(#:renamer ,it))
   (awhen (module-version module) `(#:version ,it))))

;; Normalizes keywords (#:key) and pseudo keywords (:key) used by define-module syntax.
(define (normalize-keyword kw-or-symb)
  (cond ((symbol? kw-or-symb)
          (-> (symbol->string kw-or-symb)
              (string-drop 1)
              string->keyword))
        ((keyword? kw-or-symb)
         kw-or-symb)
        (else (error "Bad keyword like" kw-or-symb))))

;; Takes one argument as taken by @code{use-modules}, or following #:use-module
;; in @code{define-module}.
;; returns a list on the form
;; (module-name (key value) ...)
;; where module name is something like (srfi srfi-1)
(define (parse-interface-specification interface-specification)
  (match interface-specification
    ;; matches `((srfi srfi-1) :select (something))
    (((parts ...) args ...)
     (apply make-module
            `(,parts ,@(concatenate
                        (map (lambda (pair)
                               (cons (normalize-keyword (car pair))
                                     (cdr pair)))
                             (group args 2))))))
    ;; matches `(srfi srfi-1)
    ((parts ...)
     (make-module parts))
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
    (((or '@ '@@) module symb)
     (list (make-module module select: (list symb))))
    ((form forms ...)
     (append (module-refer-uses form)
             (module-refer-uses forms)))
    (_ '())))

;; List of all modules pulled in in any of forms
;; Returns a list where each element suitable to have
;; resolve-interface applied to it.
(define (module-uses* forms)
  (map module->list
       (append
        (module-declaration-uses forms)
        (module-use-module-uses  forms)
        (module-refer-uses       forms))))

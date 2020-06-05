;;; Commentary:

;; This file should define all global configurable variables which
;; doesn't belong anywhere else. The config module should then import
;; this module, and set all configs as needed. The config module
;; should also be able to set configs gotten from other parts.

;;; Code:

(define-module (util config)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (ice-9 curried-definitions)
  :use-module (util)
  :use-module (util exceptions)
               )

(define-once config-values (make-hash-table))

(define-record-type <config>
  (make-config value documentation source-module attributes)
  config?
  (value get-value set-value!)
  (documentation get-documentation)
  (source-module get-source-module)
  (attributes config-attributes)
  )

(define-record-type <un-config>
  (make-unconfig value)
  unconfig?
  (value get-un-value))


;; similar to emacs defcustom
(define-macro (define-config name default-value documentation . rest)
  (let ((make-config '(@@ (util config) make-config))
        (config-values '(@@ (util config) config-values))
        (config? '(@@ (util config) config?))
        (get-value '(@@ (util config) get-value)))

    `(cond [(hashq-ref ,config-values (quote ,name))
            => (lambda (value)
                 ;; When reloading a module an already defined configuration item
                 ;; might be loaded again, just anwrap it and pretend that didn't
                 ;; happen.
                 (when (,config? value)
                   (set! value (,get-value value)))

                 (hashq-set! ,config-values (quote ,name)
                             (,make-config 'dummy ,documentation (current-module)
                                           (list ,@rest)))

                 ;; Fatal error when the default value doesn't work.
                 (catch 'config-error
                   (lambda () (set-config! (quote ,name) value))
                   (lambda (err _ fmt args __)
                     (apply (@ (util exceptions) fatal) fmt args))))]

           ;; value not set in advance
           [else
            (hashq-set! ,config-values (quote ,name)
                        (,make-config 'dummy ,documentation
                                      (current-module) (list ,@rest)))
            (catch 'config-error
              (lambda () (set-config! (quote ,name) ,default-value))
              (lambda (err _ fmt args __)
                ((@ (util exceptions) fatal) "~a ~a" fmt args)))])))


(export define-config)

(define* (config-attribute config attr optional: default)
  (aif (memv attr (config-attributes config))
       (cadr it)
       default))

(define-public ((ensure predicate) value)
  (if (not (predicate value))
      #f value))

(define-public (set-config! key value)
  (cond [(hashq-ref config-values key)
         => (lambda (conf)
              (aif (or (not value)
                       ((config-attribute conf #:pre identity)
                        value))
                   (begin
                     (set-value! conf it)
                     ((config-attribute conf #:post identity) it))

                   (throw 'config-error 'set-config!
                          "~a->~a = ~s is invalid,~%Field doc is \"~a\""
                          (module-name (get-source-module conf))
                          key value
                          (get-documentation conf))
                   ))]
        [else (hashq-set! config-values key (make-unconfig value))]))

;; unique symbol here since #f is a valid configuration value.
(define %uniq (gensym))
(define*-public (get-config key optional: (default %uniq))
  (let ((v (if (eq? default %uniq)
               (let ((v (hashq-ref config-values key %uniq)))
                 (when (eq? v %uniq)
                   (error "Missing config" key))
                 v)
               (hashq-ref config-values key default))))
    (if (config? v)
        (get-value v)
        v)))

;; (format-procedure (lambda (x y) ...)) => λx, y
;; (define (f x) ...)
;; (format-procedure f) => f(x)
(define (format-procedure proc)
  ((aif (procedure-name proc)
        (lambda (s) (string-append (symbol->string it) "(" s ")"))
        (lambda (s) (string-append "λ" s)))
   (let ((args ((@ (ice-9 session) procedure-arguments)
                proc)))
     (string-join
      (remove null?
              (list
               (awhen ((ensure (negate null?))
                       (assoc-ref args 'required))
                      (format #f "~{~a~^, ~}" it))
               (awhen ((ensure (negate null?))
                       (assoc-ref args 'optional))
                      (format #f "[~{~a~^, ~}]" it))
               (awhen ((ensure (negate null?))
                       (assoc-ref args 'keyword))
                      (format #f "key: ~{~a~^, ~}"
                              (map keyword->symbol
                                   (map car it))))
               (awhen ((ensure (negate null?))
                       (assoc-ref args 'rest))
                      (format #f "~a ..." it))))
      ", "))))

(export format-procedure)

(define-public (get-configuration-documentation)
  (define groups
    (group-by (match-lambda [(__ v)
                             (if (config? v)
                                 (get-source-module v)
                                 #f)])
              (hash-map->list list config-values )) )


  `(*TOP*
    (header "Configuration variables")
    (dl
     ,@(concatenate
        (for (module values) in groups
             `((dt "") (dd (header ,(format #f "~a" (module-name module))))
               ,@(concatenate
                  (for (key value) in values
                       `((dt ,key)
                         (dd (p (@ (inline)) ,(get-documentation value)))
                         (dt "V:")
                         (dd ,(let ((v (get-value value)))
                                (if (procedure? v)
                                    (format-procedure v)
                                    `(scheme ,v)))
                             (br)))))))))))

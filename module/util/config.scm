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
  :use-module (ice-9 curried-definitions) ; for ensure
  :use-module (util)
  :export (define-config)
)

(define-once config-values (make-hash-table))
(define-once config-properties (make-hash-table))
(hashq-set! config-properties #:description (make-object-property))
(hashq-set! config-properties #:source-module (make-object-property))
(hashq-set! config-properties #:pre (make-object-property))
(hashq-set! config-properties #:post (make-object-property))

(define-public (get-property config-name property-key)
  ((hashq-ref config-properties property-key) config-name))

(define (define-config% name default-value kwargs)
  (for (key value) in (group kwargs 2)
       (set! ((or (hashq-ref config-properties key)
                  (error "Missing config protperty slot " key))
              name)
         value))
  (set-config! name (get-config name default-value)))

(define-syntax define-config
  (syntax-rules ()
    ((_ name default kwargs ...)
     (define-config% (quote name) default
       (list source-module: (current-module)
             kwargs ...)))))

(define-public (set-config! name value)
  (hashq-set! config-values name
              (aif (get-property name #:pre)
                   (or (it value) (error "Pre crashed for" name))
                   value))

  (awhen (get-property name #:post)
         (it value)))
;; unique symbol here since #f is a valid configuration value.
(define %uniq (gensym))
(define*-public (get-config key optional: (default %uniq))
  (if (eq? default %uniq)
      (let ((v (hashq-ref config-values key %uniq)))
        (when (eq? v %uniq)
          (error "Missing config" key))
        v)
      (hashq-ref config-values key default)))

(define-public ((ensure predicate) value)
  (if (not (predicate value))
      #f value))



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


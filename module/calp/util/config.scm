;;; Commentary:

;; Configuration system.

;;; Code:

(define-module (calp util config)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 format) ; for format-procedure
  :use-module (ice-9 curried-definitions) ; for ensure
  :use-module (calp translation)
  :export (define-config)
)

(define-once config-values (make-hash-table))

;; properties declared before being bound into hash-map
;; to allow nicer scripting in this file.

(define-once config-properties (make-hash-table))
(define description (make-object-property))
(define source-module (make-object-property))
(define pre (make-object-property))
(define post (make-object-property))
(hashq-set! config-properties #:description description)
(hashq-set! config-properties #:source-module source-module)
(hashq-set! config-properties #:pre pre)
(hashq-set! config-properties #:post post)


;; Config cells "are" immutable. @var{set-property!} is
;; therefore intentionally unwritten.

(define-public (get-property config-name property-key)
  ((hashq-ref config-properties property-key) config-name))


(define (define-config% name default-value kwargs)
  (for (key value) in (group kwargs 2)
       (aif (hashq-ref config-properties key)
            (set! (it name) value)
            (scm-error 'configuration-error
                       "define-config"
                       (_ "No configuration slot named ~s, when defining ~s")
                       (list key name)
                       #f)))
  (set-config! name (get-config name default-value)))

(define-syntax define-config
  (syntax-rules ()
    ((_ name default kwargs ...)
     (define-config% (quote name) default
       (list source-module: (current-module)
             kwargs ...)))))

(define-public (set-config! name value)
  (hashq-set! config-values name
              (aif (pre name)
                   (or (it value)
                       (scm-error 'configuration-error
                                  "set-config!"
                                  ;; first slot is property name, second is new
                                  ;; property value.
                                  (_ "Pre-property failed when setting ~s to ~s")
                                  (list name value)
                                  #f))
                   value))

  (awhen (post name) (it value)))

;; unique symbol here since #f is a valid configuration value.
(define %uniq (gensym))
(define*-public (get-config key optional: (default %uniq))
  (if (eq? default %uniq)
      (let ((v (hashq-ref config-values key %uniq)))
        (when (eq? v %uniq)
          (scm-error 'configuration-error
                     "get-config"
                     (_ "No configuration item named ~s")
                     (list key) #f))
        v)
      (hashq-ref config-values key default)))



(define-public ((ensure predicate) value)
  (if (predicate value)
      value #f))



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

;; TODO break this up into separate `get-all-configuration-items' and
;; `format-configuration-items' procedures
(define-public (get-configuration-documentation)
  (define groups
    (group-by (compose source-module car)
              (hash-map->list list config-values)))

  `(*TOP*
    (header ,(_ "Configuration variables"))
    (dl
     ,@(concatenate
        (for (module values) in groups
             `((dt "") (dd (header ,(aif module
                                         (->str (module-name it))
                                         #f)))
               ,@(concatenate
                  (for (key value) in values
                       `((dt ,key)
                         (dd (p (@ (inline))
                                ,(or (description key) "")))
                         ;; Configuration variable value indicator
                         (dt ,(_ "V:"))
                         (dd ,(if (procedure? value)
                                  (format-procedure value)
                                  `(scheme ,value))
                             (br)))))))))))


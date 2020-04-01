;;; Commentary:

;; This file should define all global configurable variables which
;; doesn't belong anywhere else. The config module should then import
;; this module, and set all configs as needed. The config module
;; should also be able to set configs gotten from other parts.

;;; Code:

(define-module (util config)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (util)
               )

(define-public (ensure pred?)
  (lambda (v)
    (unless (pred? v)
      (error (format #f "Value [~s] doesn't satisfy condition ~a"
               v (or (procedure-name pred?) ""))))
    v))



(define-once config-values (make-hash-table))

(define-record-type <config>
  (make-config value documentation valid-value? source-module)
  config?
  (value get-value set-value!)
  (documentation get-documentation)
  (valid-value? get-valid-value)
  (source-module get-source-module))


;; similar to emacs defcustom
;; TODO possibly make @var{documentation} and @var{valid-value?} optional.
(define-macro (define-config name default-value documentation valid-value?)
  `(let ((make-config (@@ (util config) make-config))
         (config-values (@@ (util config) config-values)))
     (cond [(hashq-ref config-values (quote ,name))
            => (lambda (value)
                 (unless (,valid-value? value)
                   (throw 'config-error
                          "Config [~a]: ~a doesn't sattisfy predicate ~s~%\"~a\"~%"
                          (quote ,name)
                          value
                          ,valid-value?
                          ,documentation))
                 (hashq-set! config-values (quote ,name)
                             (make-config value ,documentation
                                          ,valid-value? (current-module))))]
           ;; value not set in advance
           [else
            (hashq-set! config-values (quote ,name)
                        (make-config ,default-value ,documentation
                                     ,valid-value? (current-module)))])))

(export define-config)

(define-public (set-config! key value)
  (cond [(hashq-ref config-values key)
         => (cut set-value! <> value)]
        [else (hashq-set! config-values key value)]))

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


(define-public (print-configuration-documentation)
  (define groups
    (group-by (match-lambda [(__ v)
                             (if (config? v)
                                 (get-source-module v)
                                 #f)])
              (hash-map->list list config-values )) )
  (for (module values) in groups
       (format #t "~%~a~%" (module-name module))
       (for (key value) in values
            (format #t "  ~20,a | ~a~%" key (get-documentation value)))))

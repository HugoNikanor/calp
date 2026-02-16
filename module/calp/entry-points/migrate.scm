(define-module (calp entry-points migrate)
  :export (main)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module ((ice-9 regex) :select (string-match))
  :use-module (calp translation)
  :use-module (ice-9 getopt-long)
  :use-module (srfi srfi-1)
  :use-module (vcomponent data-stores common)
  :use-module (web uri)
  )

(define-public %category 'application)

;;; Format of the configuration file
;;; --------------------------------
;;;     (call-with-input-file "migrate.conf" read)
;;; MUST return a single alist, where the following keys are checked:
;;; - stores, a list of store declarations, each on the form
;;;  `('store type . args)`, where `type` references the module
;;;  `(vcomponent data-stores ,type), whose method `create-instance` is
;;;  directly applied to the remaining arguments.
;;;
;;; Note that this file IS NOT evaluated (besides `read`)

(define opt-spec
  `((config (single-char #\c) (value file)
            (description ,(G_ "Configuration file where stores are defined")))
    (in (value #t) (single-char #\i)
        (description ,(G_ "Store to use as input, by uri or name")))
    (out (value #t) (single-char #\o)
         (description ,(G_ "Store to use as output, by uri or name")))
    (list-stores
     (value format)
     (description ,(G_ "List all configured stores, in the provided data format")))
    (help (single-char #\h)
          (description ,(G_ "Print this help.")))))


;;; TODO --transfer-metadata
;;; Currently, calendar description and the like is never read from
;;; the source calendar, and meaning that the destination calendar keeps
;;; its existing data, or its default if it's created by us


(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (define config
    (cond ((option-ref opts 'config #f)
           => (lambda (f) (call-with-input-file f read)))
          (else '())))

  ;; (format #t "CONFIG: ~s~%" config)

  (define stores
    (fold (lambda (declaration stores)
            (define name (list-ref declaration 1))
            (define type (list-ref declaration 2))

            (table-put stores name
                       (apply (module-ref (resolve-interface
                                           `(vcomponent data-stores ,type))
                                          'create-instance)
                              (drop declaration 3))))
          (table)
          (or (assoc-ref config 'stores) '())))

  (cond ((option-ref opts 'list-stores #f)
         => (lambda (fmt)
              (case fmt
                ((sexp #t)
                 ;; TODO we actually need to serialize each store
                 ;; Currently, we get read expression for a table containing opaque store objects
                 (write stores))
                (else
                 (format (current-error-port)
                         "Unknown serialization format: ~s.~%"
                         fmt)
                 ;; TODO error code
                 (throw 'return)))
              (newline)
              (throw 'return))))

  ;; TODO better error on missing --in or --out
  (define in-store  (get-store stores (option-ref opts 'in #f)))
  (define out-store (get-store stores (option-ref opts 'out #f)))
  (unless (and in-store out-store)
    ;; TODO better error
    (format (current-error-port) "ERROR: in-store or out-store missing~%")
    ;; TODO non-zero exit code
    (throw 'return))

  (format (current-error-port) "in:  ~s~%out: ~s~%"
          in-store out-store)

  ;; (format (current-error-port) "~s~%" (list-entries in-store))

  (define entries (list-entries in-store))
  (define entry-count (length entries))
  (for (idx . (href . entry)) in (enumerate entries)
       (format (current-error-port) "Migrating ~a/~a ~s~%" (1+ idx) entry-count href)
       (put-event! out-store href entry))

  (flush! out-store)
  )


(define (get-store stores descriptor)
  (typecheck stores table?)
  (typecheck descriptor string?)
  (if (string-match "^store:" descriptor)
      (-> descriptor string->uri store-uri->store)
      (table-get stores (string->symbol descriptor))))

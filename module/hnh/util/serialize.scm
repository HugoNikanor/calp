(define-module (hnh util serialize)
  :use-module ((hnh util) :select (predicate-list-get))
  :export (serialize
           serializers
           set-record-type-serializer!
           with-serializers))

;; Return a form, which when evaluated, returns the source object.
;; Compare this with "write", which outputs a string which returns the
;; source object when read back in.
;; For example `(write 'a)` would output `a`, while `(serialize 'a)`
;; would return `(quote a)`
;; A valid (but ugly) implementation of `write` would be:
;;     (define (write object port)
;;       (format port "#.~s" object))
;; assuming that the fluid `read-eval?` is set to `#t`.

(define-once serializers (make-parameter (list)))

(define (set-record-type-serializer! type-predicate serializer)
  ;; NOTE New serializers are pre-pended. This allows serializers to
  ;; be overwritten, and allows more specific serializers to be added
  ;; later. It however comes with the slight downside that `symbol?`
  ;; is one of the last serializers tested, which might make the code
  ;; slightly slower.
  (serializers (cons (cons type-predicate serializer) (serializers))))

(define (serialize object)
  (cond ((predicate-list-get (serializers) object)
         => (lambda (s) (s object)))
        ;; Assume self-quoting
        (else object)))

(set-record-type-serializer!
 symbol?
 (lambda (obj)
   (catch #t (lambda ()
               ;; A bug in Guile makes symbols which look
               ;; like floating point numbers with exponents
               ;; larger than allowed to fail to write. For
               ;; example, (string->symbol "1e500<anything>")
               ;; crashes when printed, as if `1e500` was
               ;; trying to be evaluated.
               (with-output-to-string (lambda () (write obj)))
               `(quote ,obj))
     (lambda _ `(string->symbol ,(symbol->string obj))))))

;; (set-record-type-serializer!
;;  circular-list?
;;  (lambda (obj) '(circular-lists-not-yet-supported)))


(set-record-type-serializer!
 pair?
 (lambda (pair) `(cons ,(serialize (car pair))
                  ,(serialize (cdr pair)))))

(set-record-type-serializer!
 list?
 (lambda (obj) `(list ,@(map serialize obj))))

(define-syntax with-serializers
  (syntax-rules ()
    ((_ ((pred serializer) ...)
        body ...)
     (parameterize ((serializers (cons* (cons pred serializer) ...
                                        (serializers))))
       body ...))))

(define-module (util app)
  :use-module (util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :export (make-app current-app define-method getf setf)
  )

(define-immutable-record-type <app>
  (make-app% ht) app? (ht get-ht))

(define-public (make-app)
  (make-app% (make-hash-table)))

(define current-app (make-parameter (make-app)))

(define-syntax (define-method stx)
  (with-syntax ((app (datum->syntax stx 'app)))
   (syntax-case stx ()
     [(_ (name args ...) body ...)

      (let* ((pre post (break (lambda (s) (eqv? key: (syntax->datum s)))
                              #'(args ...))))
        #`(define*-public (name #,@pre #,@(if (null? post) '(key:) post)
                         (app (current-app)))
            body ...))])))


(define (getf app field)
  (aif (hashq-ref (get-ht app) field)
       (force it)
       #f))

(define-syntax setf%
  (syntax-rules ()
    [(_ app field value)
     (hashq-set! (get-ht app) field (delay (begin value)))]))

(define-syntax setf
  (syntax-rules ()
    [(_ app) app]
    [(_ app key value rest ...)
     (begin (setf% app key value)
            (setf app rest ...))]))

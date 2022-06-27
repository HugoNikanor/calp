(define-module (hnh util lens)
  :use-module (srfi srfi-1)
  :export (modify
           modify*
           set
           get

           identity-lens
           compose-lenses
           lens-compose

           ref car* cdr*))


(define (modify object lens f . args)
  (lens object (apply f (lens object) args)))

(define-syntax modify*
  (syntax-rules ()
    ((_ object f) (f object))
    ((_ object lens rest ...)
     (modify object lens
             (lambda (object*) (modify* object* rest ...))))))

;; The simple case of getting and setting when you already have the lens is trivial
;; (lens object)
;; (lens object new-value)

(define-syntax set
  (syntax-rules ()
    ((_ object lenses ... value)
     (modify* object lenses ... (const value)))))

(define-syntax get
  (syntax-rules ()
    ((_ object) object)
    ((_ object f lenses ...)
     (get (f object) lenses ...))))




(define (make-lens getter setter)
  (case-lambda ((datum) (getter datum))
               ((datum new-value) (setter datum new-value))))

(define-syntax build-lens
  (syntax-rules ()
    ((_ (getter gargs ...)
        (setter sargs ...))
     ;; (make-lens (lambda (datum) (getter datum gargs ...))
     ;;            (lambda (datum new-value) (setter datum sargs ... new-value)))
     (case-lambda ((datum)
                   (getter datum gargs ...))
                  ((datum new-value)
                   (setter datum sargs ... new-value))))
    ((_ (getter args ...) setter)
     (build-accesor (getter args ...) (setter)))
    ((_ getter (setter args ...))
     (build-lens (getter) (setter args ...)))
    ((_ getter setter)
     (build-lens (getter) (setter)))))




(define identity-lens
  (case-lambda ((a) a)
               ((_ a) a)))

(define (compose-lenses% f g)
  (build-lens (get f g) (set f g)))

(define (compose-lenses . fs)
  (reduce-right compose-lenses% identity-lens fs))

(define lens-compose compose-lenses)



(define (list-change list index value)
  (cond ((zero? index)
         (cons value (cdr list)))
        ((null? list)
         (scm-error 'out-of-range "list-change" "" #f #f))
        (else
         (cons (car list)
               (list-change (cdr list)
                            (1- index)
                            value)))))



(define (ref idx)
  (build-lens (list-ref idx) (list-change idx)))


(define car* (make-lens car (lambda (pair value) (cons value (cdr pair)))))
(define cdr* (make-lens cdr (lambda (pair value) (cons (car pair) value))))

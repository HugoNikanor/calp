(define-module (hnh util lens)
  :use-module (srfi srfi-1)
  :use-module (ice-9 control)
  :use-module (ice-9 curried-definitions)
  :use-module (oop goops)
  :export (modify
           set
           get

           identity-lens
           compose-lens
           lens-compose

           focus-matching
           traversed

           each

           ref car* cdr*
           ))


(define ((car* lst) f)
  (cons (f (car lst))                   ; ← focus
        (cdr lst)))

(define ((cdr* lst) f)
  (cons (car lst)
        (f (cdr lst))))

(define (((ref idx) list) f)
  (let loop ((idx idx) (rem list))
    (if (zero? idx)
        (cons (f (car rem))
              (cdr rem))
        (cons (car rem)
              (loop (1- idx)
                    (cdr rem))))))

(define (((focus-matching predicate) list) f)
  (map (lambda (x)
         (if (predicate x)
             (f x)
             x))
       list))

(define-method (each (lst <list>))
  (lambda (f) (map f lst)))


;;; Lens l i :: l i → (i → i) → l i

;;; modify :: (l i, Lens l i, (i → i)) → l i
(define (modify container lens f)
  ((lens container) f))

;;; set :: (l i, Lens l i, i) → l i
(define (set container lens value)
  (modify container lens (const value)))

;;; get :: (l i, Lens l i) → i
(define (get container . lenses)
  (call/ec (lambda (return)
             (modify container (apply lens-compose lenses)
                     return))))

(define (traversed container lens)
  (define v '())
  ((lens container) (lambda (x) (set! v (cons x v))))
  v)


(define lens-compose
  (case-lambda
    ((lens)
     (lambda (object)
       (lambda (operator)
         (modify object lens operator))))
    ((lens . lenses)
     (lambda (object)
       (lambda (operator)
         (modify object lens
                 (lambda (focus) (((apply lens-compose lenses) focus) operator))))))))

(define compose-lens lens-compose)


(define ((identity-lens object) op)
  (op object))

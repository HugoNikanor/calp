(define-module (hnh util set)
  :use-module (hnh util)
  :use-module (hnh util destructure)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :export ((set . data-set)
           set?
           set-empty?
           list->set
           set->list
           set-add
           set-pop

           set-intersection
           set-union
           set-difference
           ))

(define (any->symbol x)
  (string->symbol (format #f "~s" x)))

(define-type (set serializer:
                  (lambda (s) `(list->set '(,@(set->list s))))
                  no-destructure?: #t)
  (internals default: (table)))

;;; TODO destructure pattern

(define (set-empty? x)
  (table-empty? (internals x)))

(define (list->set args)
  (set
   internals:
   (fold (lambda (x tab) (table-put tab (any->symbol x) x))
         (table)
         args)))

(define (set->list set)
  (table->list (internals set) (lambda (_ v) v)))

(define (set-add set . values)
  (modify set internals*
          (cut fold (lambda (v table)
                      (table-put table (any->symbol v) v))
               <> values)))

;;; Two functions in a trenchcoat.
;;; Called with one value, or with (nothing)
;;; then a random node is popped, and returned
;;; but if just a specific value that exact value
;;; is popped (but still returned).
(define* (set-pop set optional: (value (nothing)))
  (let ((value new-tree (destructure value
                          ((just v)
                           (table-pop (internals set)
                                      (any->symbol v)))
                          (_ (table-any (internals set))))))
    (values value (-> set (internals new-tree)))))


(define (set-union . as)
  (typecheck as (list-of set?))
  (set internals:
       (apply table-union
              (map internals as))))


(define (set-intersection . as)
  (typecheck as (list-of set?))
  (set internals:
       (apply table-intersection
              (map internals as))))

(define (set-difference a b)
  (set internals:
       (table-difference (internals a)
                         (internals b))))

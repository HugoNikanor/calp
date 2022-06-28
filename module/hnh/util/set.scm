(define-module (hnh util set)
  :use-module (hnh util object)
  :use-module (hnh util table))

(define-type (set)
  (set-data default: (make-table)))

(define (set-null) (set))

(define (set-adjoin value set)
  (modify set set-data tree-put value #t))

(define (set-disjoin value set)
  (modify set set-data tree-put value #f))

(define (in-set? set value)
  (catch 'out-of-range
    (lambda () (tree-get (set-data set) value))
    (lambda () #f)))

(define (set-fold f done set)
  (tree-fold (lambda (k v lst)
               (if v
                   (f k done)
                   done))
             done set))

(define (set->list set)
  (set-fold cons '() set))

(define (set-union set1 set2)
  (set-fold set-adjoin set1 set2))

(define (set-intersection set1 set2)
  (set-fold (lambda (v set)
              (if (in-set? v set1)
                  set1
                  (set-disjoin v set1)))
            set1 set2))

(define (set-difference set1 set2)
  (set-fold set-disjoin set1 set2))

;; (define (set-xor set1 set2))



(define-module (hnh util bimap)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (srfi srfi-88)
  :export (bimap
           bimap?
           get-left get-right
           set-left! set-right!
           remove-left! remove-right!
           bimap-clear!
           bimap->list
           ))

(define-type (bimap)
  (forward  default: (make-hash-table))
  (backward default: (make-hash-table)))

(define* (get-left table key optional: dflt)
  (typecheck table bimap?)
  (hash-ref (forward table) key dflt))

(define* (get-right table key optional: dflt)
  (typecheck table bimap?)
  (hash-ref (backward table) key dflt))

(define (set-left! table key value)
  (typecheck table bimap?)
  (let ((back-key (hash-ref (forward table) key)))
    (hash-set! (forward table) key value)
    (hash-remove! (backward table) back-key)
    (hash-set! (backward table) value key)))

(define (set-right! table key value)
  (typecheck table bimap?)
  (let ((back-key (hash-ref (backward table) key)))
    (hash-set! (backward table) key value)
    (hash-remove! (forward table) back-key)
    (hash-set! (forward table) value key)))

(define (remove-left! table key)
  (typecheck table bimap?)
  (let ((back-key (hash-ref (forward table) key)))
    (hash-remove! (forward table) key)
    (hash-remove! (backward table) back-key)))

(define (remove-right! table key)
  (typecheck table bimap?)
  (let ((back-key (hash-ref (backward table) key)))
    (hash-remove! (backward table) key)
    (hash-remove! (forward table) back-key)))

(define (bimap-clear! table)
  (typecheck table bimap?)
  (hash-clear! (backward table))
  (hash-clear! (forward table)))

(define* (bimap->list table optional: (proc cons))
  (typecheck table bimap?)
  (hash-map->list proc (forward table)))

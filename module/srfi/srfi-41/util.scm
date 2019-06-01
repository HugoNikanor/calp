(define-module (srfi srfi-41 util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (util) ; let*, find-min
  #:export (stream-car+cdr interleave-streams))

(define (stream-car+cdr stream)
  (values (stream-car stream)
          (stream-cdr stream)))

;; Merges a number of totally ordered streams into a single
;; totally ordered stream.
;; ((≺, stream)) → (≺, stream)
(define (interleave-streams < streams)
  ;; Drop all empty streams
  (let ((streams (remove stream-null? streams)))
    ;; If all streams where empty, end the output stream
    (if (null? streams)
        stream-null
        (let* ((min other (find-min < stream-car streams))
               (m ms (stream-car+cdr min)))
          (stream-cons m (interleave-streams < (cons ms other)))))))

(define-public (stream-insert < item s)
  (interleave-streams < (list (stream item) s)))

(define-public (filter-sorted-stream proc stream)
  (stream-take-while
   proc (stream-drop-while
         (negate proc) stream)))

(define-public (filter-sorted-stream* pred? keep-remaining? stream)
  (cond [(stream-null? stream) stream-null]
        [(keep-remaining? (stream-car stream)) stream]
        [(pred? (stream-car stream))
         (stream-cons (stream-car stream)
                      (filter-sorted-stream*
                       pred? keep-remaining?
                       (stream-cdr stream)))]
        [else (filter-sorted-stream* pred? keep-remaining?
                                     (stream-cdr stream))]))

(define-public (stream-find pred stream)
  (cond ((stream-null? stream) #f)
        ((pred (stream-car stream)) (stream-car stream))
        (else (stream-find pred (stream-cdr stream)))))

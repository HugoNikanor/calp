(define-module (srfi srfi-41 util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (util) ; let*, find-min
  #:export (stream-car+cdr interleave-streams with-streams))

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

;; Requires that stream is a total order in regards to what we filter
;; on. From there it knows that once it has found the first element
;; that satisfies our predicate all remaining elements satisfying pred
;; will be in direct succession.
(define-public (filter-sorted-stream pred stream)
  (stream-take-while
   pred (stream-drop-while
         (negate pred) stream)))


;; Simmilar to the regular @code{filter-sorted-stream}, but once an
;; element satisfies @code{keep-remaning?} then the remaining tail
;; of the stream is all assumed to be good.
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

;; Finds the first element in stream satisfying pred.
;; Returns #f if nothing was found
(define-public (stream-find pred stream)
  (cond ((stream-null? stream) #f)
        ((pred (stream-car stream)) (stream-car stream))
        (else (stream-find pred (stream-cdr stream)))))

;; Evaluates @var{body} in a context where most list fundamentals are
;; replaced by stream alternatives.
;; commented defifinitions are items which could be included, but for
;; one reason or another isn't.
;; TODO Possibly give access to list-primitives under a list- prefix.
(define-macro (with-streams . body)
  `(let-syntax
       ((cons        (identifier-syntax stream-cons))
        (null?       (identifier-syntax stream-null?))
        (pair?       (identifier-syntax stream-pair?))
        (car         (identifier-syntax stream-car))
        (cdr         (identifier-syntax stream-cdr))
        ;;           stream-lambda
        ;;           define-stream
        (append      (identifier-syntax stream-append))
        (concat      (identifier-syntax stream-concat))
        ;; (const    stream-constant)
        (drop        (identifier-syntax stream-drop))
        (drop-while  (identifier-syntax stream-drop-while))
        (filter      (identifier-syntax stream-filter))
        (fold        (identifier-syntax stream-fold))
        (for-each    (identifier-syntax stream-for-each))
        (length      (identifier-syntax stream-length))
        ;;           stream-let
        (map         (identifier-syntax stream-map))
        ;;           stream-match
        ;;           stream-range
        ;;           stream-ref
        (reverse     (identifier-syntax stream-reverse))
        ;;           stream-scan
        (take        (identifier-syntax stream-take))
        (take-while  (identifier-syntax stream-take-while))
        (zip         (identifier-syntax stream-zip)))
     ,@body))

(define-module (vcomponent search)
  :use-module (util)
  :use-module (ice-9 sandbox)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  )


(define (close-parenthese str)
  (define missing-parenthesis-count
    (string-fold (lambda (char count)
                   (case char
                     ((#\() (1+ count))
                     ((#\)) (1- count))
                     (else count)))
                 0 str))
  (string-append str (make-string missing-parenthesis-count #\))))

(define-public (prepare-string str)
  (call-with-input-string (close-parenthese str) read))

(define-public (build-query-proc . expressions)
  ;; TODO does this eval help? Or will the body of the procedure
  ;; be evalutade later?
  (eval `(lambda (event) ,@expressions)
        (make-sandbox-module
         `(
           ((vcomponent base) prop)
           ((ice-9 regex) string-match)
           ;; TODO datetime
           ,@all-pure-bindings)
         )))


(define-public (prepare-query query-proc event-set)
  (stream-paginate (stream-filter query-proc event-set)) )

;; TODO possibly make this procedure deny any query-procedures not created by
;; build-query-procedure
;; (event → bool), (stream event), (() → Any) → (paginated-stream event)
(define*-public (execute-query query page
                               key:
                               ;; time-out-handler
                               (time-limit 1))
  (catch 'not-an-actual-error ; 'timed-out
    (lambda ()
      (call-with-time-limit
       time-limit
       ;; Stream->list needs to be here, since the actual
       ;; stream-force needs to happen within the actual
       ;; @var{call-with-time-limit}.
       (lambda () (stream->list (stream-ref query page)))
       (lambda _ (throw 'timed-out))))
    (lambda (err . args)
      (display (cons err args) (current-error-port))
      (newline  (current-error-port))
      (case err
        ((timed-out) (aif time-out-handler (it)
                          (apply throw err 'args))))
      'timed-out                 ; when search took to long
      'unbound-variable          ; when search term has unbound variables
      'wrong-type-arg            ;; stream-ref
      '())))

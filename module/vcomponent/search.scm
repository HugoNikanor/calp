(define-module (vcomponent search)
  :use-module (util)
  :use-module (ice-9 sandbox)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  )


;; Takes a string and appends closing parenthese until all parenthese are
;; closed.
(define (close-parenthese str)
  (define missing-parenthesis-count
    (string-fold (lambda (char count)
                   (case char
                     ((#\() (1+ count))
                     ((#\)) (1- count))
                     (else count)))
                 0 str))
  (string-append str (make-string missing-parenthesis-count #\))))

;; Prepares a string to be sent to build-query-proc
;; sexp-like string -> sexp
(define-public (prepare-string str)
  (call-with-input-string (close-parenthese str) read))

;; Evaluates the given expression in a sandbox.
;; NOTE Should maybe be merged inte prepare-query. The argument against is that
;; eval-in-sandbox is possibly slow, and that would prevent easy caching by the
;; caller.
;; sexp -> (event → bool)
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

;; execute a query procedure created by build-query-proc.
;; (event → bool), int, (optional int) → (list event) throws 'timed-out
(define* (execute-query query page key: (time-limit 1))
  (call-with-time-limit
   time-limit
   ;; Stream->list needs to be here, since the actual
   ;; stream-force needs to happen within the actual
   ;; @var{call-with-time-limit}.
   (lambda () (stream->list (stream-ref query page)))
   (lambda _ (format (current-error-port) "~a~%" 'timed-out)
      (throw 'timed-out)))
)


;; Creates a prepared query wrappend in a paginator.
;; (event → bool), (stream event) → <paginator>
(define*-public (prepare-query query-proc event-set optional: (page-size 10))
  (make-paginator (stream-paginate
                   (stream-timeslice-limit
                    (stream-filter query-proc event-set))
                   page-size)))

(define-record-type <paginator>
  (make-paginator% query max-page true-max-page?)
  paginator?
  (query get-query) ; (paginated-stream event)
  (max-page get-max-page set-max-page!) ; int
  (true-max-page? true-max-page? set-true-max-page!))

(export paginator? get-query get-max-page true-max-page?)

(define (make-paginator query)
  (make-paginator% query 0 #f))

;; a fancy version of 1+ which caps at max page
;; <paginator>, int → int
(define*-public (next-page paginator optional: (page (get-max-page paginator)))
  (if (true-max-page? paginator)
      (min (1+ page) (get-max-page paginator))
      (1+ page)))

(define-public (paginator->list paginator proc tail-proc)
  (if (true-max-page? paginator)
      (map proc (iota (1+ (get-max-page paginator))))
      (append (map proc (iota (1+ (get-max-page paginator))))
              (list (tail-proc (next-page paginator))))))


(define*-public (paginator->sub-list paginator current-page proc
                                     key: head-proc tail-proc
                                     (ahead 5) (behind 5)
                                     )

  (let ((start (max 0 (- current-page behind)))
        (end (min (+ current-page ahead)
                  (get-max-page paginator))))

    (display (head-proc start))
    (for-each proc (iota (1+ (- end start)) start))
    (display (tail-proc end)))

  )

;; returns the contents of the requested page, or throws 'max-page with the
;; highest known available page.
;; <paginator>, int → (list event) throws ('max-page <int>)
(define-public (get-page paginator page)
  (catch 'timed-out
    (lambda () (let ((result (execute-query (get-query paginator) page)))
            (set-max-page! paginator (max page (get-max-page paginator)))
            (when (> 10 (length result))
              (set-true-max-page! paginator #t))
            result))
    (lambda (err . args)
      (set-max-page! paginator (get-max-page paginator))
      (set-true-max-page! paginator #t)
      (throw 'max-page (get-max-page paginator)))))



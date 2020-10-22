;;; Commentary:

;; Procedures for searching in a (possibly) infinite stream. Everything is general,
;; with the exception of @var{build-query-proc}, which is tailored for searches on
;; sets on vcomponents.

;; > TODO since most of this module is generic, break it out and only have the
;; > vcomponent-specific parts here.

;; A search isn't guaranteed to include all available objects, since each object
;; only has a limited time to get found. This is mostly a problem if the matches
;; are /really/ far from one another.
;; NOTE a system of continuations to allow a search to be resumed with a higher
;; timeout would be cool to have.

;; Currently all searches is assumed to go through prepare-query and the paginator
;; interface. It shouldn't however be a problem to work with the flat result-set
;; returned by @var{execute-query} directly.

;; @var{<paginator>} isn't strictly necessary even for paginated queries, since the
;; evaluation time and pagination is baked into the stream. It is however useful
;; for keeping track of the number of available pages, and if we have found the
;; "final" element.

;;; Code:

(define-module (vcomponent search)
  :use-module (calp util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module ((ice-9 sandbox)
               :select (make-sandbox-module
                        all-pure-bindings)))


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
           ((vcomponent base) prop param children type)
           ((ice-9 regex) string-match)
           ;; TODO datetime
           ,@all-pure-bindings)
         )))


;; Returns a new stream which is the result of filtering the input set with the
;; query procedure.
;; (a → bool), (stream a) → (stream a)
(define-public (execute-query query-proc event-set)
  (stream-timeslice-limit
   (stream-filter query-proc event-set)
   ;; .5s, tested on my laptop. .1s sometimes doesn't get to events on
   ;; 2020-08-10, where the first event is on 1974-12-02.
   0.5))

;; Creates a prepared query wrappend in a paginator.
;; (event → bool), (stream event) → <paginator>
(define*-public (prepare-query query-proc event-set optional: (page-size 10))
  (make-paginator (stream-paginate (execute-query query-proc event-set)
                                   page-size)))

(define-record-type <paginator>
  (make-paginator% query max-page true-max-page?)
  paginator?
  (query get-query) ; (paginated-stream event)
  (max-page get-max-page set-max-page!) ; int
  (true-max-page? true-max-page? %set-true-max-page!))

(define (set-true-max-page! paginator)
  (%set-true-max-page! paginator #t))

(define (unset-true-max-page! paginator)
  (%set-true-max-page! paginator #f))

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
  (catch 'wrong-type-arg
    (lambda () (let ((q (get-query paginator)))
            (if (stream-null? q)
                (begin
                  (set-true-max-page! paginator)
                  '())
                (let ((result (stream->list
                               (stream-ref (get-query paginator) page))))
                  ;; This check isn't strictly necessary, but without it
                  ;; we always needs to force the next page. And since this
                  ;; page is "incomplete" we already know that this is the
                  ;; final page.
                  (when (> 10 (length result))
                    (set-true-max-page! paginator))

                  (set-max-page! paginator (max page (get-max-page paginator)))
                  result))))
    (lambda (err proc fmt args data)
      ;; TODO ensure the error actually is index out of range.
      ;; (format (current-error-port) "~?~%" fmt args)
      (set-max-page! paginator (get-max-page paginator))
      (set-true-max-page! paginator)
      (throw 'max-page (get-max-page paginator))
      )))



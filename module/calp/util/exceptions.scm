(define-module (calp util exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (calp util)
  #:use-module (calp util config)
  #:use-module (ice-9 format)
  #:export (throw-returnable
            catch-multiple
            assert))

(define-syntax-rule (throw-returnable symb args ...)
  (call/cc (lambda (cont) (throw symb cont args ...))))

;; Takes a (non nested) list, and replaces all single underscore
;; symbols with a generated symbol. For macro usage.
(define (multiple-ignore lst)
  (map/dotted (lambda (symb) (if (eq? symb '_) (gensym "ignored_") symb))
              lst))

;; Like @var{catch}, but multiple handlers can be specified.
;; Each handler is on the form
;; @example
;; [err-symb (args ...) body ...]
;; @end example
;; 
;; Only errors with a handler are caught. Error can *not* be given as
;; an early argument.
(define-macro (catch-multiple thunk . cases)
  (let catch-recur% ((errs (map car cases)) (cases cases))
    (let* ((v (car errs))
           (case other (partition (lambda (case) (eq? v (car case))) cases))
           (g!rest (gensym "rest")))
      `(catch (quote ,v)
         ,(if (null? (cdr errs))
              thunk
              `(lambda () ,(catch-recur% (cdr errs) other)))
         (lambda (err . ,g!rest)
           (apply (lambda ,(let ((param-list (second (car case))))
                        (if (not (pair? param-list))
                            param-list
                            (multiple-ignore param-list)))
                    ,@(cddr (car case)))
                  ,g!rest))))))



(define-public warning-handler
  (make-parameter
   (lambda (fmt . args)
     (format #f "WARNING: ~?~%" fmt args))))

(define-public warnings-are-errors
  (make-parameter #f))

(define-config warnings-are-errors #f
  description: "Crash on warnings."
  post: warnings-are-errors)

;; forwards return from warning-hander. By default returns an unspecified value,
;; but instances are free to provide a proper return value and use it.
(define-public (warning fmt . args)
  (display (apply (warning-handler) fmt (or args '()))
           (current-error-port))
  (when (warnings-are-errors)
    (throw 'warning fmt args)))

(define-public (fatal fmt . args)
  (display (format #f "FATAL: ~?~%" fmt (or args '()))
           (current-error-port))
  (raise 2)
  )

(define (prettify-tree tree)
  (cond [(pair? tree) (cons (prettify-tree (car tree))
                            (prettify-tree (cdr tree)))]
        [(and (procedure? tree) (procedure-name tree))
         => identity]
        [else tree]))



(define-macro (assert form)
  `(unless ,form
     (throw 'assertion-error "Assertion for ~a failed, ~a"
            (quote ,form)
            ((@@ (calp util exceptions) prettify-tree) ,(cons 'list form)))))


(define-syntax catch-warnings
  (syntax-rules ()
    ((_ default body ...)
     (parametrize ((warnings-are-errors #t))
       (catch 'warning
         (lambda ()
           body ...)
         (lambda _ default))))))

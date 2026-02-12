;;; Commentary:
;;; Expandable match
;;; Code:

(define-module (hnh util destructure)
  ;; Limit the amount of (hnh ...) imports here here,
  ;; since very many of them pull in us.
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :export (match-expanders
           define-matcher
           get-expander
           common-sequence-destructurer
           destructure
           destructure-lambda
           destructure-lambda*))

;;; TODO this really should be a parameter,
;;; to allow local overrides of the available patterns
(define-once match-expanders (make-hash-table))

(define-syntax (define-matcher stx)
  (syntax-case stx ()
    ((_ (name args ...) declarations ...)
     #`(hash-set! match-expanders (quote name)
                  (lambda (stx)
                    (syntax-case stx (name)
                      ((name args ...) (let () declarations ...))))))))

(define (get-expander stx)
  (syntax-case stx ()
    ((inner inner-args ...)
     (cond ((hash-ref match-expanders (syntax->datum #'inner))
            => (lambda (inner-expander)
                 (inner-expander #'(inner inner-args ...))))
           ;; This is a compile time error
           (else (scm-error 'type-error "destructure"
                            "No registered pattern for `~a'"
                            (list (syntax->datum #'inner)) #f))))

    (x (identifier? #'x)
       (values (const #'()) list #'(x)))
    (x (values (lambda (expr) #`((equal? x #,expr)))
               (const #'()) #'()))))

;; emacs: (put 'destructure 'scheme-indent-function 1)
;; emacs: (font-lock-add-keywords 'scheme-mode '(("(\\(\\<destructure\\>\\)" (1 font-lock-keyword-face)))
(define-syntax (destructure stx)
  (syntax-case stx ()
    ((_ expr cases ...)
     #`(let ((v expr))
         (cond
          #,@(for case in #'(cases ...)
                  (syntax-case case ()
                    ((pattern body1 body ...)
                     (let ()
                      (define-values (predicates emitted-values captures)
                        (get-expander #'pattern))
                      (with-bindings (predicates #'v)
                                     (emitted-values #'v)
                                     captures
                                     #'(body1 body ...))))))

          ;; Run time error
          (else (scm-error 'match-error "destructure" "no match for ~s"
                           (list v) #f)))))))

(define (group-bindings captures emitted)
    (map (lambda (group)
           (cons (car group)
                 (map cdr (cdr group))))
         ;; Manual remove instead of assoc-remove! Since equivalence
         ;; of syntax objects is weird
         (remove (lambda (group)
                   (eq? '_ (syntax->datum (car group))))
                 (group-by car (map cons captures emitted)))))

;;; predicates :: "inner predicates"
;;; emitted :: "inner values"
;;; captures :: "inner captures"
;;; body :: code to run with these bindings available
;;;         must be a list of forms
;;; Returns a 2-list, consisting of a predicate, and a body
;;; can be inserted into a cond form, or spliced into an and form
;;; NOTE maybe we should export this, to allow others to
;;; easily create `and` like expanders
(define (with-bindings predicates emitted captures body)
  (define groups
    (group-bindings captures emitted))

  #`((and #,@predicates
          ;; For each group with more than one instance
          ;; Check that all instances are equal
          #,@(filter-map (lambda (group)
                           (and (< 1 (length (cdr group)))
                                #`(equal? #,@(cdr group))))
                         groups))
     (let #,(map (lambda (group)
                   #`(#,(car group) #,(cadr group)))
                 groups)
       #,@body)))




(define-syntax-rule (destructure-lambda clause ...)
  (lambda (arg) (destructure arg clause ...)))

(define-syntax-rule (destructure-lambda* clause ...)
  (lambda args (destructure args clause ...)))




(define-matcher (@ name inner)
  ;; TODO explicit error if `name` doesn't satisfy `identifier?`
  ;; Currently it expands, and yields a 'bad let in form' error
  (let ((inner-predicates inner-values inner-captures
                          (get-expander #'inner)))
    (values
     inner-predicates
     (lambda (expr) #`(#,expr #,@(inner-values expr)))
     #`(name #,@inner-captures))))

(define-matcher 'sexp
  (values
   (lambda (expr) #`((equal? 'sexp #,expr)))
   (const #'())
   #'()))

;;; TODO this is so broken
;; (define-matcher `sexp
;;   (let proc ((stx #'sexp))
;;     (syntax-case stx (unquote unquote-slicing)
;;       (,x (get-expander #'x))
;;       (,@x (get-expander #`(list #,@#'x)))
;;       ;; ((init ... . tail)  (get-expander #`(cons* `init ... tail)))
;;       ;; ;; TODO this assumes ellipsis support
;;       (#(xs ...) (get-expander #`(vector `xs ...)))
;;       (x (get-expander #''x)))))


(define-matcher (and matcher predicates ...)
  (define-values (inner-predicates inner-vals captures)
    (get-expander #'matcher))
  (values
   (lambda (expr)
     (with-bindings (inner-predicates expr)
                    (inner-vals expr)
                    captures
                    #'((and predicates ...))))
   inner-vals
   captures))

(define-matcher (cons a d)
  (define-values (car-preds car-vals car-captures)
    (get-expander #'a))
  (define-values (cdr-preds cdr-vals cdr-captures)
    (get-expander #'d))
  (values
   (lambda (expr)
     #`((pair? #,expr)
        #,@(car-preds #`(car #,expr))
        #,@(cdr-preds #`(cdr #,expr))))
   (lambda (expr)
     (append (car-vals #`(car #,expr))
             (cdr-vals #`(cdr #,expr))))
   (append car-captures cdr-captures)))


;; Explicit syntax, to use the built in pattern matcher
(hash-set! match-expanders 'cons*
           (lambda (stx)
             (syntax-case stx (cons*)
               ((cons* x)
                (get-expander #'x))
               ((cons* x xs ...)
                (get-expander #'(cons x (cons* xs ...)))))))





;; returns 3 values:
;; - the list of fields before the ellipsised field
;; - the ellipsised field
;; - the list of fields after the ellipsised field
;; 
;; If no elippsises field exists, then the second value is #f, and the
;; third value the empty list.
;; 
;; Examples:
;; > (find-ellipsis #'(a b))       => (values #'(a b) #f '())
;; > (find-ellipsis #'(a b ... c)) => (values #'(a) #'b #'(c))
;; > (find-ellipsis #'(b ...))     => (values '() #'b '())
;; > (find-ellipsis #'(a ... b))   => (values '() #'a #'(b))
;; (note that the above examples might require use of replaced
;; ellipsis (through `find-ellipsis`)).
;;
;; The form `(... x)` "works", in that it's parsed as `(a x)`
;; (e.g. `...` is treated as a variable).
(define (find-ellipsis lst)
  ;; Destructure is NOT used in the implementation.
  ;; Partly to avoid dependency loops, and party because
  ;; matching on syntax object isn't (yet) supported.
  (let loop ((before '())
             (remaining lst))
    (cond ((null? remaining)
           (values (reverse before)
                   #f '()))
          ((null? (cdr remaining))
           (values (reverse (cons (car remaining) before))
                   #f '()))
          ((eq? '... (syntax->datum (cadr remaining)))
           (values (reverse before) (car remaining) (cddr remaining)))
          (else
           (loop (cons (car remaining) before)
                 (cdr remaining))))))





;; Helper procedure used by `common-sequence-destructer` to guarantee
;; that all keyword arguments are supplied.
(define (missing symbol)
  (scm-error 'wrong-type-arg #f "Missing required keyword #:~a"
             (list symbol) (list (symbol->keyword symbol))))

;; ref (front and back) and slice are guaranteed to use indices inside
;; the container, as declared through the length procedure.
;; 
;; Traversable t => ; Not really traversable, since we send the whole interface here
;;   container-predicate         ; x -> boolean (x is of type `t`)
;;   container-length            ; t x -> integer
;;   container-every             ; (x -> boolean), t x -> boolean
;;   container-slice             ; t x, start, end -> t x
;;   container-ref-front         ; t x, int -> x
;;   container-ref-back          ; t x, int -> x
;;   container-map               ; x -> y, t x -> t y
(define* (common-sequence-destructurer
          args                         ; destructure pattern arguments
          key:
          (container-predicate (missing 'container-predicate))
          (container-length    (missing 'container-length))
          (container-every     (missing 'container-every))
          (container-slice     (missing 'container-slice))
          (container-ref-front (missing 'container-ref-front))
          (container-ref-back  (missing 'container-ref-back))
          (container-map       (missing 'container-map)))

  (define-values (before ellipsised after)
    (find-ellipsis args))

  (when (memv '... (syntax->datum after))
    (scm-error 'syntax-error "destructure"
               "Multiple ellipsis patterns not allowed at same level"
               '() #f))

  (define before-inners
    (map (lambda (x) (values->vector (get-expander x)))
         before))

  (cond ((not ellipsised)
         ;; Special case when we don't have an ellipsised element.
         ;; Logically, this is identical to the below code, except
         ;; that container length is exact here, and a lower bound
         ;; below. This however generates much cleaner code, since we
         ;; don't try to work on the (here empty) slice which the
         ;; ellipsised part would have matched.
         (values (lambda (expr)
                   #`((#,container-predicate #,expr)
                      (= #,(length before-inners) (#,container-length #,expr))
                      #,@(append-map (lambda (i v) ((vector-ref v 0)
                                               #`(#,container-ref-front #,expr #,i)))
                                     (iota (length before))
                                     before-inners)))
                 (lambda (expr)
                   (append-map (lambda (i v) ((vector-ref v 1)
                                         #`(#,container-ref-front #,expr #,i)))
                               (iota (length before))
                               before-inners))
                 (append-map (lambda (v) (vector-ref v 2)) before-inners)))

        (else
         (define after-inners
           (map (lambda (x) (values->vector (get-expander x)))
                after))

         (define-values (ellipsised-predicate ellipsised-values ellipsised-bindings)
           (get-expander ellipsised))

         (values
          ;; predicates
          (lambda (expr)
            #`((#,container-predicate #,expr)
               (>= (#,container-length #,expr)
                  #,(+ (length before-inners)
                       (length after-inners)))

               ;; befores
               #,@(append-map (lambda (i v) ((vector-ref v 0)
                                        #`(#,container-ref-front #,expr #,i)))
                              (iota (length before))
                              before-inners)
               ;; afters
               #,@(append-map (lambda (i v) ((vector-ref v 0)
                                        #`(#,container-ref-back #,expr #,i)))
                              (iota (length after) (1- (length after)) -1)
                              after-inners)

               ;; ellipsised
               (#,container-every (lambda (x) (and #,@(ellipsised-predicate #'x)))
                                  (#,container-slice
                                   #,expr
                                   #,(length before)
                                   (- (#,container-length #,expr)
                                      #,(length after))))))

          ;; bound values
          (lambda (expr)
            #`(
               ;; befores
               #,@(append-map (lambda (i v) ((vector-ref v 1)
                                        #`(#,container-ref-front #,expr #,i)))
                              (iota (length before))
                              before-inners)

               ;; afters
               #,@(append-map (lambda (i v) ((vector-ref v 1)
                                        #`(#,container-ref-back #,expr #,i)))
                              (iota (length after) (1- (length after)) -1)
                              after-inners)

               ;; ellipsised
               #,@(map (lambda (extractor)
                         #`(#,container-map (lambda (x) #,extractor)
                                            (#,container-slice
                                             #,expr
                                             #,(length before)
                                             (- (#,container-length #,expr)
                                                #,(length after)))))
                       (ellipsised-values #'x))))


          ;; identifiers
          (append
           (append-map (lambda (v) (vector-ref v 2))
                       (append before-inners after-inners))
           ellipsised-bindings)))))





(define-matcher (list args ...)
  (common-sequence-destructurer
   #'(args ...)
   container-predicate: #'list?
   container-length: #'length
   container-every: #'every
   container-slice: #'list-slice
   container-ref-front: #'list-ref
   container-ref-back: #'list-ref-back
   container-map: #'map))

(define-matcher (vector args ...)
  (common-sequence-destructurer
   #'(args ...)
   container-predicate: #'vector?
   container-length: #'vector-length
   container-every: #'(@ (srfi srfi-43) vector-every)
   container-slice: #'vector-copy
   container-ref-front: #'vector-ref
   container-ref-back: #'vector-ref-back
   container-map: #'(lambda (f v) ((@ (srfi srfi-43) vector-map) (lambda (_ x) (f x)) v))))

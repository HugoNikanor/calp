(define-module (calp html components)
  :use-module (hnh util)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 match)
  :use-module (calp translation)
  :export (xhtml-doc
           slider-input
           btn
           include-css
           include-alt-css
           input-plus-minus
           ))

;; Wraps a number of sxml forms into a valid sxhtml-tree.
(define-syntax xhtml-doc
  (syntax-rules (@)
    ((_ (@ attr ...) body ...)
     `(*TOP*
       (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
       ,(lambda () (format #t "~%<!DOCTYPE html>~%"))
       (html (@ (xmlns "http://www.w3.org/1999/xhtml") attr ...)
             body ...)))
    ((_ body ...)
     (xhtml-doc (@) body ...))))


;; Add a slider with an associated number input. Keeps the two in sync.
(define* (slider-input key: variable
                       (min 0)
                       (max 10)
                       (step 1)
                       (value 1)
                       (unit ""))

  `(slider-input
    (@ (min ,min)
       (max ,max)
       (step ,step)
       (value ,value)
       (oninput
        ,(format #f "document.documentElement.style.setProperty('--~a', this.value + '~a')"
                 variable unit)))))

;; Generates a button or button-like link.
(define* (btn key: onclick href (class '())
              allow-other-keys:
              rest: args)
  (when (and onclick href)
    (scm-error 'wrong-type-arg "btn"
               (_ "href and onclick are mutually exclusive. href = ~s, onclick = ~s.")
               (list href onclick)
               #f))

  (let ((body #f))
    `(,(cond [href 'a]
             [else 'button])
      (@ (class ,(string-join (cons "btn" class) " "))
         ,@(cond [onclick `((onclick ,onclick))]
                 [href `((href ,href))]
                 [else '()])
         ,@(let loop ((rem args))
             (cond
              [(null? rem) '()]
              [(memv (car rem) '(onclick: href: class:))
               (loop (cddr rem))]
              [(keyword? (car rem))
               (cons* `(,(keyword->symbol (car rem))
                        ,(cadr rem))
                      (loop (cddr rem)))]
              [else
               (set! body (car rem))
               (loop (cdr rem))])))
      ,body)))


(define ((set-attribute attr) el)
  (match el
    [(tagname ('@ params ...) inner-body ...)
     `(,tagname (@ ,@(assq-merge params attr))
                ,@inner-body)]
    [(tagname inner-body ...)
     `(,tagname (@ ,attr)
                ,@inner-body)]))


(define (include-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,path)
            ,@extra-attributes)))


(define (include-alt-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "alternate stylesheet")
            (href ,path)
            ,@extra-attributes)))


(define (input-plus-minus positive?)
  (define id (gensym "id"))
  `(span (@ (class "input-timespan"))
         (input (@ (type "checkbox")
                   (style "display:none")
                   (class "plusminuscheck")
                   ,@(if positive? '((checked)) '())
                   (id ,id)))
         (label
          (@ (for ,id))
          (span (@ (class "plus"))  "+")
          (span (@ (class "minus")) "-"))))

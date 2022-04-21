(define-module (calp html components)
  :use-module (hnh util)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 match)
  :use-module (calp translation)
  :export (xhtml-doc)
  )

;; Wraps a number of sxml forms into a valid sxhtml-tree.
(define-syntax xhtml-doc
  (syntax-rules (@)
    ((_ (@ attr ...) body ...)
     `(*TOP*
       (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
       (html (@ (xmlns "http://www.w3.org/1999/xhtml") attr ...)
             body ...)))
    ((_ body ...)
     (xhtml-doc (@) body ...))))


;; Add a slider with an associated number input. Keeps the two in sync.
(define*-public (slider-input key: variable
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
;; TODO <div/> inside <button/> isn't valid.
(define*-public (btn key: onclick href (class '())
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


;; Creates a group of tabs from a given specification. The specification
;; @var{elements} should be a list, where each element is a sublist on
;; the form
;; @example
;; ("tab icon" arguments ... tab-body)
;; @end example
;; where arguments are zero or more pairs of keyword arguments. For example:
;; @example
;; ("📅" title: "Översikt" ,(fmt-single-event ev))
;; @end example
;; Creates a tab with an calendar emoji as icon, "Översikt" is sent as the
;; extra argument #:title, and the body is the return from fmt-single-event.
(define-public (tabset elements)
  (define tabgroup (symbol->string (gensym "tabgroup")))

  `(div (@ (class "tabgroup"))
        ,@(for (i (key args ... body)) in (enumerate elements)
               (define id (symbol->string (gensym "tab")))
               `(div (@ (class "tab"))
                     (input (@ (type "radio") (id ,id) (name ,tabgroup)
                               ,@(when (zero? i) '((checked)))))
                     ;; It would be preferable to place the labels in a separate
                     ;; div and set that to have fixed position, since we could
                     ;; then just flow them. That hovever doesn't work since we
                     ;; need a css-selector for the label to the selected radio
                     ;; option.
                     (label (@ ,@(assq-merge `((for ,id)
                                               (style "top: calc(var(--tab-size) * " ,i ")"))
                                             (kvlist->assq args)))
                            ,key)
                     (div (@ (class "content")) ,body)))))

(define ((set-attribute attr) el)
  (match el
    [(tagname ('@ params ...) inner-body ...)
     `(,tagname (@ ,@(assq-merge params attr))
                ,@inner-body)]
    [(tagname inner-body ...)
     `(,tagname (@ ,attr)
                ,@inner-body)]))


(define-public (with-label lbl . forms)

  (define id (gensym "label"))

  (cons `(label (@ (for ,id)) ,lbl)
   (let recurse ((forms forms))
     (map (lambda (form)
            (cond [(not (list? form)) form]
                  [(null? form) '()]
                  [(eq? 'input (car form))
                   ((set-attribute `((id ,id))) form)]
                  [(list? (car form))
                   (cons (recurse (car form))
                         (recurse (cdr form)))]
                  [else
                   (cons (car form)
                         (recurse (cdr form)))]))
          forms))))


(define-public (form elements)
  `(form
    ,@(map (label self
                  (lambda (el)
                    (match el
                      ((name ('@ tags ...) body ...)
                       (let ((id (gensym "formelement")))
                         (cons
                          `(label (@ (for ,id)) ,name)
                          (map
                           (set-attribute `((name ,name)))
                           (cons
                            ((set-attribute `((id ,id))) (car body))
                            (cdr body))))))
                      ((name body ...)
                       (self `(,name (@) ,@body))))))
           elements)))


(define-public (include-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,path)
            ,@extra-attributes)))


(define-public (include-alt-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "alternate stylesheet")
            (href ,path)
            ,@extra-attributes)))


(define-public (input-plus-minus positive?)
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

;; (define-once timespan-generator-id (gensym "timespan-generator"))
;; (define-public (input-timespan-generator)
;;   `((div (@ (class "template")
;;             (id ,timespan-generator-id))
;;          ,(input-timespan))
;;     (script
;;      "function make_timespan_input() {\n"
;;      "return document.getElementsById(" ,timespan-generator-id ").cloneNode(true);")))

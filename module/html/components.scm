(define-module (html components)
  :use-module (util)
  :use-module (util exceptions)
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


;; Add a slider with an associated number input. Keeps the two in check.
;; Uses the js function setVar (which must be provided elsewhere)
;; set the the value of @var{variable}.
(define*-public (slider-input key: variable
                              (min 0)
                              (max 10)
                              (step 1)
                              (value 1)
                              (unit ""))
  (let ((groupname (symbol->string (gensym "slider"))))
    `(div (@ (class "input-group"))
          (script
           "function " ,groupname "fn (value) {"
           "setVar('" ,variable "', value + '" ,unit "');"
           "for (let el of document.getElementsByClassName('" ,groupname "')) {"
           "    el.value = value;"
           "}}")
          (input (@ (type "range")
                    (class ,groupname)
                    (min ,min)
                    (max ,max)
                    (step ,step)
                    (value ,value)
                    (oninput ,groupname "fn(this.value)")
                    ))
          (input (@ (type "number")
                    (class ,groupname)
                    (min ,min)
                    (max ,max)
                    (step ,step)
                    (value ,value)
                    (oninput ,groupname "fn(this.value)"))
                 ))))

;; Generates a button or button-like link.
;; TODO <div/> inside <button/> isn't valid.
(define*-public (btn key: onclick href (class '())
              allow-other-keys:
              rest: args)
  (when (and onclick href)
    (error "Only give one of onclick, href and submit."))

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
      (div ,body))))


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
                     (label (@ (for ,id) (style "top: " ,(* 6 i) "ex")
                               ,(awhen (memv title: args)
                                       `(title ,(cadr it))))
                            ,key)
                     (div (@ (class "content")) ,body)))))


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

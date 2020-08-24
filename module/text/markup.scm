(define-module (text markup)
  :use-module (calp util)
  :use-module (srfi srfi-1)
  :use-module (ice-9 match)
  :use-module (ice-9 pretty-print)
  :use-module (text util)
  :use-module (text flow)
  :use-module (texinfo string-utils))


(define (esc . effect)
  (format #f "\x1b[~am"
          (if (null? effect)
              ""
              (case (car effect)
                [(bold) 1]
                [(italic) 3]
                [(invert) 7]
                [else 4]))))

;; tag := (tag-name [(@ attributes ...)] body ...)

;; alist → tag → tag
(define (add-attributes args)
  (match-lambda
    [(name ('@ tagargs ...) body ...)
     `(,name (@ ,@(assq-limit (assq-merge tagargs args)))
             ,@body)]
    [(name body ...)
     `(,name (@ ,@args) ,@body)]
    [nonlist nonlist]))


(define (get-attr args key default)
  (aif (assoc-ref args key)
       (car it) default))

;; NOTE Some tags can be given a `width' attribute. This is however not yet
;; fully supported.
(define* (ontree tag body optional: (args '()))
  (case tag
    [(*TOP* group block) (string-concatenate
                          (map (compose sxml->ansi-text (add-attributes args))
                               body))]
    [(header) (sxml->ansi-text `(group (center (@ ,@args) (b ,@body)) (br)))]
    [(center) (center-string (string-concatenate (map sxml->ansi-text body))
                             (get-attr args 'width 70))]
    [(p) (string-append (string-join (flow-text (string-concatenate (map sxml->ansi-text body))
                                                width: (get-attr args 'width 70))
                                     "\n")
                        (if (assoc-ref args 'inline) "" "\n\n")
                        )]
    [(b) (string-append (esc 'bold) (string-concatenate (map sxml->ansi-text body)) (esc))]
    [(i em) (string-append (esc 'italic) (string-concatenate (map sxml->ansi-text body)) (esc))]
    ;; NOOP, but for future use.
    [(code) (string-concatenate (map sxml->ansi-text body))]
    [(blockquote) (string-concatenate
                   (map (lambda (line) (sxml->ansi-text `(group (ws (@ (minwidth 4))) ,line (br))))
                        (flow-text
                         (string-concatenate (map sxml->ansi-text body))
                         width: 66)))]
    [(ws) (make-string  (aif (assoc-ref args 'minwidth)
                             (car it) 1)
                        #\space)]
    [(br) "\n"]
    [(hr) (string-append "     " (make-string 60 #\─) "     \n")]
    [(dl)
     (let* ((dts dds (partition (lambda (x) (eq? 'dt (car x))) body)))
       (let* ((dts* (map sxml->ansi-text dts))
              (m (if (null? dts*) 0 (apply max (map true-string-length dts*)))))
         (string-concatenate
          (map (lambda (dt dd)
                 (let ((dds (string-split dd #\newline)))
                   (string-concatenate
                    (map (lambda (left right)
                           (string-append (true-string-pad left m) " │ " right "\n"))
                         (cons dt (map (const "") (iota (1- (length dds)))))
                         dds))))
               dts*
               (map (compose sxml->ansi-text (add-attributes `((width ,(- 70 m 5)))))
                    dds)))))]
    [(dt) (string-concatenate (map (compose sxml->ansi-text (add-attributes args))
                                   body))]
    [(dd)
     (string-concatenate
      (map (compose sxml->ansi-text (add-attributes args))
           body))]

    [(scheme)
     (string-concatenate
      (map (lambda (form)
             (string-trim-both
              (with-output-to-string
                (lambda () (pretty-print form width: (aif (assoc-ref args 'width) (car it) 70))))))
           body))]

    [else (string-append (esc 'bold) "??"
                         "`"
                         (esc 'invert)
                         (string-concatenate (map sxml->ansi-text body))
                         (esc) "'")]
    )
  )

(define (onleaf leaf)
  (format #f "~a" leaf))

(define (parse-tree tree-callback leaf-callback)
  (match-lambda
    [(tag ('@ args ...) body ...)
     (tree-callback tag body args)]
    [(tag body ...)
     (tree-callback tag body)
     ]
    [() ""]
    [(any ...) (map leaf-callback any)]
    [any (leaf-callback any)]))


(define-public (sxml->ansi-text tree)
  ((parse-tree ontree onleaf) tree))

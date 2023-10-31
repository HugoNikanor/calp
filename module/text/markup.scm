(define-module (text markup)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (ice-9 match)
  :use-module (ice-9 pretty-print)
  :use-module (text util)
  :use-module (text flow)
  :use-module (texinfo string-utils)
  :export (sxml->ansi-text))

;; Takes an HTML-like sxml coded tree, and produces a string with
;; appropriate spacing and ANSI-escapes for different tags.
(define (sxml->ansi-text tree)
  ((parse-tree ontree onleaf) tree))


(define (esc . effect)
  (format #f "\x1b[~am"
          (if (null? effect)            ; NOCOV
              ""
              (case (car effect)        ; NOCOV
                [(bold) 1]
                [(italic) 3]
                [(invert) 7]
                [else (scm-error 'misc-error "esc"
                                 "Unknown escape: ~s"
                                 (car effect) #f)]))))

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

(define (dt? x)
  (and (list? x) (eq? 'dt (car x))))

(define (dd? x)
  (and (list? x) (eq? 'dd (car x))))

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
                         ;; TODO shouldn't this use (- width 4)?
                         width: 66)))]
    [(ws) (make-string  (aif (assoc-ref args 'minwidth)
                             (car it) 1)
                        #\space)]
    [(br) "\n"]
    ;; TODO width
    [(hr) (string-append "     " (make-string 60 #\─) "     \n")]
    [(dl)
     (let* ((body
             (map (lambda (el)
                    (cond ((dt? el) (list 'dt (sxml->ansi-text el)))
                          ((dd? el) el)
                          (else (scm-error 'wrong-type-arg "ontree"
                                           "Only <dt/> and <dd/> are valid children of <dl/>, got ~s"
                                           (list el)
                                           #f))))
                  body))
            (dt-width (apply max 0 (map (compose true-string-length cadr) (filter dt? body)))))
       (let loop ((remaining body)
                  (dts '()))
         (if (null? remaining)
             (string-join
              (map string-append
                   (append (map (lambda (x) (true-string-pad x dt-width))
                                (reverse dts)))
                   (make-list (length dts) " │ "))
              "\n" 'suffix)
             (let ((el (car remaining)))
               (cond ((dt? el) (loop (cdr remaining) (cons (cadr el) dts)))
                     ((dd? el)
                      (let ((content (lines
                                      (sxml->ansi-text `(block (@ (width ,(- 70 dt-width 3)))
                                                               ,el)))))
                        (string-append
                         (unlines
                          (map string-append
                               (append (map (lambda (x) (true-string-pad x dt-width))
                                            (reverse dts))
                                       (make-list (max 0 (- (length content)
                                                            (length dts)))
                                                  (make-string dt-width #\space)))
                               (make-list (max (length content) (length dts)) " │ ")
                               (append content (make-list (max 0 (- (length dts) (length content)))
                                                          ""))))
                         "\n"
                         (loop (cdr remaining) '()))))
                     (else (scm-error 'misc-error "on-tree" "Unexpected: ~s~%" (list (car remaining)) #f  )))))))]
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
                         (with-output-to-string
                           (lambda () (write body)))
                         (esc) "'")]))

(define (onleaf leaf)
  (format #f "~a" leaf))

(define (parse-tree tree-callback leaf-callback)
  (match-lambda
    [(tag ('@ args ...) body ...)
     (tree-callback tag body args)]
    [(tag body ...)
     (tree-callback tag body)]
    [() ""]
    [(any ...) (map leaf-callback any)]
    [any (leaf-callback any)]))



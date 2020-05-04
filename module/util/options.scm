(define-module (util options)
  :use-module (util)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module ((output text) :select (flow-text)))

;; option-assoc → getopt-valid option-assoc
(define-public (getopt-opt options)
  (map (lambda (optline)
         (cons (car optline)
               (lset-intersection (lambda (a b) (eqv? b (car a)))
                                  (cdr optline)
                                  '(single-char required? value predicate))))
       options))




;; (name (key value) ...) → sxml
(define (fmt-help option-line)
  (let ((name (car option-line))
        (args (cdr option-line)))
    (let ((valuefmt (case (and=> (assoc-ref args 'value) car)
                      [(#t) '("=" (i value))]
                      [(#f) '()]
                      [else => (lambda (s) `("[=" (i ,s) "]"))])))
      `(*TOP* (b "--" ,name) ,@valuefmt
              ,@(awhen (assoc-ref args 'single-char)
                       `("," (ws)
                         (b "-" ,(car it)) 
                         ,@valuefmt))
              (br)
              ,@(awhen (assoc-ref args 'description)
                       `((blockquote ,(car it))
                         (br)))))))

(define (esc . effect)
  (format #f "\x1b[~am"
          (if (null? effect)
              ""
              (case (car effect)
                [(bold) 1]
                [(italic) 3]
                [(invert) 7]
                [else 4]))))

(use-modules (texinfo string-utils))

;; NOTE width is hard coded to 70 chars
(define* (ontree tag body optional: (args '()))
  (case tag
    [(*TOP* group) (string-concatenate (map sxml->ansi-text body))]
    [(center) (center-string (string-concatenate (map sxml->ansi-text body)) 70)]
    [(p) (string-append (string-join (flow-text (string-concatenate (map sxml->ansi-text body))
                                                width: 70)
                                     "\n")
                        "\n\n")]
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
    [(hr) (string-append "     " (make-string 60 #\_) "     \n")]
    [else (string-append (esc 'invert) (string-concatenate (map sxml->ansi-text body)) (esc))]
    )
  )

(define (onleaf leaf)
  (format #f "~a" leaf))

(define (parse-tree tree-callback leaf-callback)
  (match-lambda
    [(tag ('@ (key value) ...) body ...)
     (tree-callback tag body
                    (zip key value) )]
    [(tag body ...)
     (tree-callback tag body)
     ]
    [() ""]
    [(any ...) (map leaf-callback any)]
    [any (leaf-callback any)]))


(define-public (sxml->ansi-text tree)
  ((parse-tree ontree onleaf) tree))

(define-public (format-arg-help options)
  (sxml->ansi-text (cons '*TOP* (map sxml->ansi-text (map fmt-help options)))))

(define*-public (print-arg-help options optional: (port (current-error-port)))
  (display (format-arg-help options) port))

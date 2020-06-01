(define-module (util options)
  :use-module (util)
  :use-module (srfi srfi-1)
)

;; option-assoc → getopt-valid option-assoc
(define-public (getopt-opt options)
  (map (lambda (optline)
         (cons (car optline)
               (map (lambda (opt-field)
                      (cons (car opt-field)
                            (cond [(and (eq? 'value (car opt-field))
                                        (symbol? (cadr opt-field)))
                                   '(optional)]
                                  [else (cdr opt-field)])))
                    (lset-intersection (lambda (a b) (eqv? b (car a)))
                                       (cdr optline)
                                       '(single-char required? value predicate)))))
       options))




;; (name (key value) ...) → sxml
(define (fmt-help option-line)
  (let ((name (car option-line))
        (args (cdr option-line)))
    (let ((valuefmt (case (and=> (assoc-ref args 'value) car)
                      [(#t) '(" " (i value))]
                      [(#f) '()]
                      [else => (lambda (s) `(" [" (i ,s) "]"))])))
      `(*TOP* (b "--" ,name) ,@valuefmt
              ,@(awhen (assoc-ref args 'single-char)
                       `("," (ws)
                         (b "-" ,(car it)) 
                         ,@valuefmt))
              (br)
              ,@(awhen (assoc-ref args 'description)
                       `((blockquote ,@it)
                         (br)))))))

(use-modules (text markup))


(define-public (format-arg-help options)
  (sxml->ansi-text (cons '*TOP* (map sxml->ansi-text (map fmt-help options)))))

(define*-public (print-arg-help options optional: (port (current-error-port)))
  (display (format-arg-help options) port))

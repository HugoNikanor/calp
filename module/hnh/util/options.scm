(define-module (hnh util options)
  :use-module (hnh util)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module (text markup)
  )

;; option-assoc → getopt-valid option-assoc
(define-public (getopt-opt options)
  (define ice-9-names '(single-char required? value predicate))
  (for (option-name flags ...) in options
       (cons option-name
             (map (match-lambda
                    (('value (_ ...))       `(value #t))
                    (('value (? symbol? _)) `(value optional))
                    ((key v)                `(,key ,v)))
                  (filter (match-lambda ((key _ ...) (memv key ice-9-names)))
                          flags)))))


;; (name (key value) ...) → sxml
(define (fmt-help option-line)
  (match option-line
    ((name args ...)
     (let ((valuefmt (match (assoc-ref args 'value)
                       [(#t) '(" " (i value))]
                       [(or #f (#f)) '()]
                       [(('options options ...))
                        `(" {" ,(string-join options "|") "}")]
                       [(s) `(" [" (i ,s) "]")])))
       `(*TOP* (b "--" ,name) ,@valuefmt
               ,@(awhen (assoc-ref args 'single-char)
                        `("," (ws)
                          (b "-" ,(car it))
                          ,@valuefmt))
               (br)
               ,@(awhen (assoc-ref args 'description)
                        `((blockquote ,@it)
                          (br))))))))

(define-public (format-arg-help options)
  (sxml->ansi-text (cons '*TOP* (map sxml->ansi-text (map fmt-help options)))))

(define*-public (print-arg-help options optional: (port (current-error-port)))
  (display (format-arg-help options) port))

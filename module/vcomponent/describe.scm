(define-module (vcomponent describe)
  :use-module (util)
  :use-module (vcomponent base)
  :use-module (text util))

(define*-public (describe vcomponent optional: (indent 0))
  (define ii (make-string indent #\space))
  (define iii (make-string (1+ indent) #\space))

  (define maxlen (find-max (hash-map->list
                            (lambda (a _) (string-length (symbol->string a)))
                            (properties vcomponent))))

  (format #t "~aBEGIN ~a~%" ii (type vcomponent))

  (hash-for-each (lambda (key values)
                   (define (out vline)
                    (format #t "~a~a = ~a"
                            iii
                            (trim-to-width (symbol->string key) maxlen)
                            (trim-to-width
                             (format #f "~a" (value vline))
                             (- 80 indent maxlen)))
                    (awhen (parameters vline)
                           (display " ;")
                           (for (key value) in it
                                (format #t " ~a=~a" key value)))
                    (newline))
                   (if (list? values)
                       (for-each out values)
                       (out values)))
                 (properties vcomponent))

  (for child in (children vcomponent)
       (describe child (+ indent 2)))

  (format #t "~aEND   ~a~%" ii (type vcomponent)))

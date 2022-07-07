(define-module (vcomponent util describe)
  :use-module (hnh util)
  :use-module ((srfi srfi-1) :select (car+cdr))
  :use-module (srfi srfi-71)
  :use-module (vcomponent base)
  :use-module (text util)
  :export (describe))

(define* (describe vcomponent optional: (indent 0))
  (define ii (make-string indent #\space))
  (define iii (make-string (1+ indent) #\space))

  (define maxlen (find-max (map
                            (lambda (a) (string-length (symbol->string a)))
                            (map car (properties vcomponent)))))

  (format #t "~aBEGIN ~a~%" ii (type vcomponent))

  (for-each (lambda (kv)
              (let ((key values (car+cdr kv)))
                (define (out vline)
                  (format #t "~a~a = ~a"
                          iii
                          (trim-to-width (symbol->string key) maxlen)
                          (trim-to-width
                           (format #f "~a" (value vline))
                           (- 80 indent maxlen)))
                  (awhen (vline-source vline)
                         (display ((@@ (vcomponent formats ical parse) get-line) it)))
                  (unless (null? (parameters vline))
                    (display " ;")
                    (for (key value) in (parameters vline)
                         (format #t " ~a=~a" key value)))
                  (newline))
                (if (list? values)
                    (for-each out values)
                    (out values))))
            (sort* (properties vcomponent)
                   string<?
                   ;; TODO is key always a symbol?
                   (compose symbol->string car)))

  (for child in (children vcomponent)
       (describe child (+ indent 2)))

  (format #t "~aEND   ~a~%" ii (type vcomponent)))

(define-module (hnh util path)
  :use-module (srfi srfi-1)
  :use-module (hnh util))

;; TODO shouldn't this use `file-name-separator-string'?
(define-public (path-append . strings)
  (fold (lambda (s done)
            (string-append
             done
             (if (string-null? s)
                 (string-append s "/")
                 (if (char=? #\/ (string-last done))
                     (if (char=? #\/ (string-first s))
                         (string-drop s 1) s)
                     (if (char=? #\/ (string-first s))
                         s (string-append "/" s))))))
        (let ((s (car strings)))
          (if (string-null? s)
              "/" s))
        (cdr strings)))

(define-public (path-join lst) (apply path-append lst))

;; @example
;; (path-split "usr/lib/test")
;; ⇒ ("usr" "lib" "test")
;; (path-split "/usr/lib/test")
;; ⇒ ("" "usr" "lib" "test")
;; (path-split "//usr////lib/test")
;; ⇒ ("" "usr" "lib" "test")
;; @end example
(define-public (path-split path)
  (let* ((head tail
               (car+cdr
                (reverse
                 (map reverse-list->string
                      (fold (lambda (c done)
                              (if (file-name-separator? c)
                                  (cons '() done)
                                  (cons (cons c (car done)) (cdr done))))
                            '(())
                            (string->list path)))))))
    (cons head (remove string-null? tail))))

(define-module (hnh util path)
  :use-module (srfi srfi-1)
  :use-module (hnh util))

(define // file-name-separator-string)
(define /? file-name-separator?)

(define-public (path-append . strings)
  (fold (lambda (s done)
          (string-append
           done
           (cond ((string-null? s) //)
                 ((and (/? (string-first s))
                       (/? (string-last done)))
                  (string-drop s 1))
                 ((or (/? (string-first s))
                      (/? (string-last done)))
                  s)
                 (else (string-append // s)))))
        ;; If first component is empty, add a leading slash to make
        ;; the path absolute. This isn't exactly correct if we have
        ;; drive letters, but on those system the user should make
        ;; sure that the first component of the path is non-empty.
        (let ((s (car strings)))
          (if (string-null? s)
              // s))
        (cdr strings)
        ))

(define-public (path-join lst) (apply path-append lst))

;; @example
;; (path-split "usr/lib/test")
;; ⇒ ("usr" "lib" "test")
;; (path-split "usr/lib/test/")
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
                              (if (/? c)
                                  (cons '() done)
                                  (cons (cons c (car done)) (cdr done))))
                            '(())
                            (string->list path)))))))
    (cons head (remove string-null? tail))))


(define-public (file-hidden? path)
  (define base (basename path))
  (and (not (string-null? base))
       (char=? #\. (string-ref base 0))))

(define-module (hnh util path)
  :use-module (srfi srfi-1)
  :use-module (hnh util))

(define-public (path-append . strings)
  (fold (lambda (s done)
            (string-append
             done
             (if (string-null? s)
                 (string-append s file-name-separator-string)
                 (if (file-name-separator? (string-last done))
                     (if (file-name-separator? (string-first s))
                         (string-drop s 1) s)
                     (if (file-name-separator? (string-first s))
                         s (string-append file-name-separator-string s))))))
        ;; If first component is empty, add a leading slash to make
        ;; the path absolute. This isn't exactly correct if we have
        ;; drive letters, but on those system the user should make
        ;; sure that the first component of the path is non-empty.
        (let ((s (car strings)))
          (if (string-null? s)
              file-name-separator-string s))
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

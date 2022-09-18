(define-module (hnh util path)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (hnh util)
  :export (path-append
           path-absolute?
           path-join
           path-split
           file-hidden?
           filename-extension
           realpath
           relative-to))

(define // file-name-separator-string)
(define /? file-name-separator?)

(define path-absolute? absolute-file-name?)

;; TODO remove intermidiate period components
(define (path-append . strings)
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

(define (path-join lst) (apply path-append lst))

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
(define (path-split path)
  (let ((head tail
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


(define (file-hidden? path)
  (define base (basename path))
  (and (not (string-null? base))
       (char=? #\. (string-ref base 0))))

(define (filename-extension filename)
  (let ((components (-> filename
                        ;; Path split removes potential trailing directory separator
                        path-split last
                        basename
                        (string-split #\.))))
    (if (>= 1 (length components))
        "" (last components))))

(define (realpath filename)
  (unless (string? filename)
    (scm-error 'wrong-type-arg "realpath"
               "filename not a string: ~a"
               (list filename) #f))
  (when (string-null? filename)
    (scm-error 'wrong-type-arg "realpath"
               "filename can't be empty"
               #f #f))

  (if (absolute-file-name? filename)
      filename
      (path-append (getcwd) filename)))


(define (relative-to base path)
  ;; (typecheck base string?)
  ;; (typecheck path string?)

  (when (string-null? base)
    (error "Base can't be empty" ))

  (let ((base (if (absolute-file-name? base)
                  base
                  (path-append (getcwd) base))))

    (cond ((equal? '("") base) path)
          ((not (absolute-file-name? path))
           (path-append base path))
          (else
           (let loop ((a (path-split base))
                      (b (path-split path)))
             (cond
              ((null? a) (path-join b))
              ((null? b) path)
              ((string=? (car a) (car b)) (loop (cdr a) (cdr b)))
              (else
               (path-join
                (append
                 (make-list (length a) "..")
                 (drop b (length a)))))))))))

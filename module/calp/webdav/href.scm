(define-module (calp webdav href)
  :use-module (hnh util type)
  :use-module (srfi srfi-1)
  :export (href->string
           string->href
           href-relative))


(define (href->string href)
  (typecheck href (list-of string?))

  (if (null? href)
      "/" (string-join href "/" 'prefix)))

(define (string->href s)
  (typecheck s string?)

  (remove string-null?
          (string-split s #\/)))

;; parent must be the head of child, elements in child after that is "free range"
(define (href-relative parent child)
  (typecheck parent (list-of string?))
  (typecheck child (list-of string?))

  (cond ((null? parent) child)
        ((null? child) (scm-error 'misc-error "href-relative" "Not a sub-href" '() #f))
        ((equal? (car parent) (car child))
         (href-relative (cdr parent) (cdr child)))
        (else (scm-error 'misc-error "href-relative" "Not a sub-href" '() #f))))

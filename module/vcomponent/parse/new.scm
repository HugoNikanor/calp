(define-module (vcomponent parse new)
  :use-module (util)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (vcomponent base)
 )


;; (define f (open-input-file (car (glob "~/.local/var/cal/Calendar/c17*"))))
;; (parse (map tokenize (read-file file)))

;; port → (list string)
(define (read-file port)
 (let loop ((done '()))
   (let ((line (read-line port)))
     (if (eof-object? line)
         (reverse! done)
         (loop
          (if (char=? #\space (string-ref line 0))
              (cons (string-append (car done)
                                   (string-drop line 1))
                    (cdr done))
              (cons line done)))))))

;; (list string) → (list (key kv ... value))
(define (tokenize line)
  (define colon-idx (string-index line #\:))
  (define semi-idxs
    (let loop ((idx 0))
      (aif (string-index line #\; idx colon-idx)
           (cons it (loop (1+ it)))
           (list colon-idx (string-length line)))))
  (map (lambda (start end)
         (substring line (1+ start) end))
       (cons -1 semi-idxs)
       semi-idxs))


;; (parse-itemline '("DTEND" "TZID=Europe/Stockholm" "VALUE=DATE-TIME" "20200407T130000"))
;; ⇒ #<<vline> value: "20200407T130000" parameters: #<hash-table 7f4294c913a0 2/31>>
(define (parse-itemline itemline)
  (define all
    (reverse
     (let loop ((rem (cdr itemline)))
       (if (null? (cdr rem))
           rem                             ; (list (car rem))
           (let* ((kv (car rem))
                  (idx (string-index kv #\=)))
             (cons (cons (string->symbol (substring kv 0 idx))
                         ;; NOTE handle value parsing here?
                         (substring kv (1+ idx)))
                   (loop (cdr rem))))))))

  (make-vline% (car all) (alist->hashq-table (cdr all))))



(use-modules (srfi srfi-9))
(define-record-type <component>
  (make-component% type children attributes parent)
  component?
  (type type)
  (children children)
  (attributes attributes))

(define (make-component args)
  (let* ((type (car args))
         (children attributes (partition component? (cdr args))))
    (make-component% type children attributes)))

;; (list (key kv ... value)) → <vcomponent>
(define (parse lst)
  (let loop ((lst lst)
             (stack '()))
    (if (null? lst)
        stack
        (let ((head (car lst)))
          (cond [(string=? "BEGIN" (car head))
                 (loop (cdr lst) (cons (list (string->symbol (cadr head))) stack))]
                [(string=? "END" (car head))
                 (loop (cdr lst)
                       (let* ((frame (reverse (car stack)))
                              (component (make-component frame)))
                         (if (null? (cdr stack))
                             component
                             (cons (cons component (cadr stack))
                                   (cddr stack)))))]
                [else
                 (loop (cdr lst)
                       (cons (cons (parse-itemline head)
                                   (car stack))
                             (cdr stack)))])))))

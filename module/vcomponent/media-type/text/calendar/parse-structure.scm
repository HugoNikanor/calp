;;; Commentary:
;;; Parse a bytevector into a structured list of content lines.
;;; Code:
(define-module (vcomponent media-type text calendar parse-structure)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 iconv)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module ((scheme base) :select (bytevector-length
                                      bytevector-u8-ref
                                      make-bytevector))
  :use-module ((rnrs bytevectors) :select (bytevector-copy!))
  :use-module ((rnrs bytevectors gnu) :select (bytevector-slice))

  :use-module (vcomponent media-type text calendar parse-types)

  :export (bytevector->unfolded-lines
           parse-content-line))




(define* (memchr bv u8 optional: (idx 0))
  (let loop ((idx idx))
    (cond ((= idx (bytevector-length bv)) #f)
          ((= u8 (bytevector-u8-ref bv idx)) idx)
          (else (loop (1+ idx))))))


(define* (reverse-bv-list->string bvs key: (encoding "UTF-8"))
  (let* ((len (apply + (map bytevector-length bvs)))
         (dest (make-bytevector len)))
    (let loop ((bvs bvs)
               (ptr len))
      (if (null? bvs)
          (bytevector->string dest encoding)
          (let ((bvlen (bytevector-length (car bvs))))
            (bytevector-copy! (car bvs) 0
                              dest (- ptr bvlen)
                              bvlen)
            (loop (cdr bvs) (- ptr bvlen)))))))

;;; Returns 3 values.
;;; - the index of the start of the next line, or #f if all data is consumed
;;; - the unfolded line
;;; - the number of found linebreaks
(define* (bytevector-get-unfolded-line bv optional: (start 0))
  (define CR (char->integer #\return))
  (let loop ((idx start)
             (linebreaks 0)
             (frags '()))
    (cond ((memchr bv (char->integer #\newline) idx)
           => (lambda (i)
                (let* ((slice
                        ;; Get line contents, trimming the optional CR
                        (bytevector-slice
                         bv idx (- i idx (if (= CR (bytevector-u8-ref bv (- i 1)))
                                             1 0))))
                       (rest-args (list (1+ linebreaks)
                                        (cons slice frags))))
                  (cond
                   ((= (1+ i) (bytevector-length bv))
                    (apply values #f rest-args))
                   ((memv (bytevector-u8-ref bv (1+ i))
                       (map char->integer '(#\space #\tab)))
                    (apply loop (+ i 2) rest-args))
                   (else
                    (apply values (1+ i) rest-args))))))
          (else
           ;; File missing trailing newline, use remaining data as final line
           (values #f linebreaks (cons (bytevector-slice bv idx) frags))))))


(define* (bytevector->unfolded-lines
          bv key:
          (encoding "UTF-8")
          filename)
  (let loop ((idx 0) (line 1))
    (let ((idx linebreaks frags (bytevector-get-unfolded-line bv idx)))
      (cons (logical-line
             line: line
             file: filename
             content: (reverse-bv-list->string frags encoding: encoding))
            (if idx (loop idx (+ line linebreaks)) '())))))




(define (parse-content-line line-obj)
  (typecheck line-obj logical-line?)
  (content-line
   meta: line-obj
   data:
   (call-with-input-string (logical-line-content line-obj)
     (lambda (p)
       (define property-name (read-delimited ";:" p 'peek))
       (cons property-name
             (case (read-char p)
               ((#\:) (list (read-delimited "" p)))
               ((#\;)
                (let loop ()
                  (define parameter-name (read-delimited "=" p))
                  (define parameter-value
                    (if (char=? #\" (peek-char p))
                        (begin (read-char p)
                               (read-delimited "\"" p))
                        (read-delimited ";:" p 'peek)))
                  (cons (cons parameter-name parameter-value)
                        (case (read-char p)
                          ((#\:) (list (read-delimited "" p)))
                          ((#\;) (loop))))))))))))

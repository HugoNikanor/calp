(define-module (vcomponent parse types)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (util base64)
  :use-module (rnrs io ports)
  :use-module (datetime)
  :use-module (srfi srfi-9 gnu)
  )

;; BINARY
(define (parse-binary props value)
  ;; p 30
  (unless (string=? "BASE64" (hashq-ref props 'ENCODING))
    (warning "Binary field not marked ENCODING=BASE64"))

  (base64->bytevector
   (string->bytevector value (make-transcoder (latin-1-codec)))))

;; BOOLEAN
(define (parse-boolean props value)
  (cond
   [(string=? "TRUE" value) #t]
   [(string=? "FALSE" value) #f]
   [else (warning "~a invalid boolean" value)]))

;; CAL-ADDRESS ⇒ uri

;; DATE
(define (parse-date props value)
  (parse-ics-date value))

;; DATE-TIME
(define (parse-datetime props value)
  (get-datetime
   (parse-ics-datetime
    value (hashq-ref props 'TZID #f))))

;; DURATION
(define (parse-duration props value)
  ((@ (vcomponent duration) parse-duration)
   value))

;; FLOAT
;; Note that this is overly permissive, and flawed.
;; Numbers such as @expr{1/2} is accepted as exact
;; rationals. Some floats are rounded.
(define (parse-float props value)
  (string->number value))


;; INTEGER
(define (parse-integer props value)
  (let ((n (string->number value)))
    (unless (integer? n)
      (warning "Non integer as integer"))
    n))

;; PERIOD
(define (parse-period props value)
  (let* (((left right) (string-split value #\/)))
    ;; TODO timezones? VALUE=DATE?
    (cons (parse-ics-datetime left)
          ((if (memv (string-ref right 0)
                  '(#\P #\+ #\-))
               (@ (vcomponent duration) parse-duration)
               parse-ics-datetime)
           right))))

;; RECUR
(define (parse-recur props value)
  ((@ (vcomponent recurrence parse) parse-recurrence-rule) value))

;; TEXT
;; TODO quoted strings
(define (parse-text props value)
  (let loop ((rem (string->list value))
             (str '())
             (done '()))
    (if (null? rem)
        (let ((final (reverse-list->string str)))
         (if (null? done)
             final
             (cons final done)))
        (case (car rem)
          [(#\\)
           (case (cadr rem)
             [(#\n #\N) (loop (cddr rem) (cons #\newline str) done)]
             [(#\; #\, #\\) => (lambda (c) (loop (cddr rem) (cons c str) done))]
             [else => (lambda (c) (warning "Non-escapable character: ~a" c)
                         (loop (cddr rem) str done))])]
          [(#\,)
           (loop (cdr rem) '() (cons (reverse-list->string str) done))]
          [else
           (loop (cdr rem) (cons (car rem) str) done)]))))


;; TIME
(define (parse-time props value)
  ;; TODO time can have timezones...
  (parse-ics-time value))

;; URI
(define (parse-uri props value)
  value)

(define-immutable-record-type <utc-offset>
  (make-utc-offset pm hour minute second)
  utc-offset?
  (pm offset-pm)
  (hour offset-hour)
  (minute offset-minute)
  (second offset-second))

;; UTC-OFFSET
(define (parse-utc-offset props value)
  (make-utc-offset
   (string->symbol (substring value 0 1))
   (string->number (substring value 1 3))
   (string->number (substring value 3 5))
   (if (= 7 (string-length value))
       (string->number (substring value 5 7))
       0)))

(define type-parsers (make-hash-table))
(hashq-set! type-parsers 'BINARY parse-binary)
(hashq-set! type-parsers 'BOOLEAN parse-boolean)
(hashq-set! type-parsers 'CAL-ADDRESS parse-uri)
(hashq-set! type-parsers 'DATE parse-date)
(hashq-set! type-parsers 'DATE-TIME parse-datetime)
(hashq-set! type-parsers 'DURATION parse-duration)
(hashq-set! type-parsers 'FLOAT parse-float)
(hashq-set! type-parsers 'INTEGER parse-integer)
(hashq-set! type-parsers 'PERIOD parse-period)
(hashq-set! type-parsers 'RECUR parse-recur)
(hashq-set! type-parsers 'TEXT parse-text)
(hashq-set! type-parsers 'TIME parse-time)
(hashq-set! type-parsers 'URI parse-uri)
(hashq-set! type-parsers 'UTC-OFFSET parse-utc-offset)

(define-public (get-parser type)
  (or (hashq-ref type-parsers type #f)
      (error "No parser for type" type)))

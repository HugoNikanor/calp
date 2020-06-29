;; see (vcomponent parse types)
(define-module (output types)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (util base64)
  :use-module (datetime)
  :use-module (datetime util))


(define (write-binary _ value)
  (bytevector->base64-string value))

(define (write-boolean _ value)
  (if value "TRUE" "FALSE"))

(define (write-date _ value)
  (date->string value "~Y~m~d"))

(define (write-datetime param value)
  (datetime->string (hashq-ref param 'X-HNH-ORIGINAL value)
                    ;; TODO ~Z ?
                    "~Y~m~dT~H~M~S~Z"
                    #;
                    (let ((tz (and=> (param vline 'TZID) car)))
                    (when (and tz (string= tz "UTC"))
                      (display #\Z)))))

;; TODO
(define (write-duration _ value)
  (warning "DURATION writer not yet implemented")
  (with-output-to-string
    (lambda () (write value))))

(define (write-float _ value)
  (number->string value))

(define (write-integer _ value)
  (number->string value))

;; TODO
(define (write-period _ value)
  (warning "PERIOD writer not yet implemented")
  (with-output-to-string
    (lambda () (write value))))

(define (write-recur _ value)
  ((@ (vcomponent recurrence internal)
      recur-rule->rrule-string) value))

(define (escape-chars str)
  (define (escape char)
    (string #\\ char))
  (string-concatenate
   (map (lambda (c)
          (case c
            ((#\newline) "\\n")
            ((#\, #\; #\\) => escape)
            (else => string)))
        (string->list str))))

(define (write-text _ value)
  (escape-chars value))

(define (write-time _ value)
  (time->string value "~H~M~S"))

(define (write-uri _ value)
  value)


(use-modules (datetime timespec))

(define (write-utc-offset _ value)
  (with-output-to-string
    (lambda ()
      (display (if (time-zero? (timespec-time value))
                   '+ (timespec-sign value)))
      (display (time->string (timespec-time value) "~H~M"))
      (when (not (zero? (second (timespec-time value))))
        (display (time->string (timespec-time value) "~S"))))))


(define type-writers (make-hash-table))
(hashq-set! type-writers 'BINARY write-binary)
(hashq-set! type-writers 'BOOLEAN write-boolean)
(hashq-set! type-writers 'CAL-ADDRESS write-uri)
(hashq-set! type-writers 'DATE write-date)
(hashq-set! type-writers 'DATE-TIME write-datetime)
(hashq-set! type-writers 'DURATION write-duration)
(hashq-set! type-writers 'FLOAT write-float)
(hashq-set! type-writers 'INTEGER write-integer)
(hashq-set! type-writers 'PERIOD write-period)
(hashq-set! type-writers 'RECUR write-recur)
(hashq-set! type-writers 'TEXT write-text)
(hashq-set! type-writers 'TIME write-time)
(hashq-set! type-writers 'URI write-uri)
(hashq-set! type-writers 'UTC-OFFSET write-utc-offset)

(define-public (get-writer type)
  (or (hashq-ref type-writers type #f)
      (error "No writer for type" type)))

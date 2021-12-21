;; see (vcomponent parse types)
(define-module (vcomponent formats ical types)
  :use-module (calp util)
  :use-module (calp util exceptions)
  :use-module (base64)
  :use-module (datetime)
  :use-module (datetime timespec))

;; TODO shouldn't these really take vline:s?

(define (write-binary _ value)
  (bytevector->base64-string value))

(define (write-boolean _ value)
  (if value "TRUE" "FALSE"))

(define (write-date _ value)
  (date->string value "~Y~m~d"))

(define (write-datetime param value)
  ;; NOTE We really should output TZID from param here, but
  ;; we first need to change so these writers can output
  ;; parameters.
  (datetime->string (hashq-ref param '-X-HNH-ORIGINAL value)
                    "~Y~m~dT~H~M~S~Z"))

(define (write-duration _ value)
  ((@ (vcomponent duration) format-duration) value))

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

(define-public (escape-chars str)
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

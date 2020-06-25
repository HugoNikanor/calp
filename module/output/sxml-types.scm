(define-module (output sxml-types)
  :use-module (util)
  :use-module (output types)
  :use-module (datetime)
  :use-module (datetime util)
  )

(define (write-boolean _ v)
  `(boolean ,(if v "true" "false")))

(define (write-date _ value)
  `(date ,(date->string v "~Y-~m-~d")))

(define (write-datetime p v)
  ;; TODO TZID?
  (datetime->string
   (hashq-ref p 'X-HNH-ORIGINAL v)
   ;; TODO ~z?
   "~Y-~m-~dT~H:~M:~S~Z"))

(define (write-time _ v)
  (time->string v "~H:~M:S"))

(define (write-recur _ v)
  `(recur ,@(recur-rule->rrule-sxml v)))

;; sepparate since this text shouldn't be escaped
(define (write-text _ v)
  ;; TODO out type should be xsd:string.
  ;; Look into what that means, and escape
  ;; from there
  `(text ,v))



(define sxml-writers (make-hash-table))
(for simple-type in '(BINARY DURATION CAL-ADDRESS DURATION FLOAT INTEGER
                             #| TODO PERIOD |# URI UTC-OFFSET)
     (hashq-set! sxml-writers simple-type
                 (lambda (p v)
                   `(,(downcase-symbol simple-type)
                     ,((get-writer simple-type) p v)))))

(hashq-set! sxml-writers 'BOOLEAN write-boolean)
(hashq-set! sxml-writers 'DATE write-date)
(hashq-set! sxml-writers 'DATE-TIME write-datetime)
(hashq-set! sxml-writers 'TIME write-time)
(hashq-set! sxml-writers 'RECUR write-recur)
(hashq-set! sxml-writers 'TEXT write-text)

(define-public (get-writer type)
  (or (hashq-ref sxml-writers type #f)
      (error "No writer for type" type)))

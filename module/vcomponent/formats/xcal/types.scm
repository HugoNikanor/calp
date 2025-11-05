(define-module (vcomponent formats xcal types)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (datetime)
  :use-module (calp translation)
  :use-module ((calp namespaces) :select (xcal))
  :use-module ((sxml namespaced) :select (xml))
  :export (get-writer))

(define (write-boolean _ v)
  ((xml xcal 'boolean) (if v "true" "false")))

(define (write-date _ v)
  ((xml xcal 'date) (date->string v "~Y-~m-~d")))

(define (write-datetime p v)
  ((xml xcal 'date-time)
   (datetime->string
    (or (table-get p '-X-HNH-ORIGINAL v) v)
    ;; 'Z' should be included for UTC,
    ;; other timezones MUST be specified
    ;; in the TZID parameter.
    "~Y-~m-~dT~H:~M:~S~Z")))

(define (write-time _ v)
  ((xml xcal 'time) (time->string v "~H:~M:S")))

(define (write-recur _ v)
  (apply (xml xcal 'recur)
         ((@@ (vcomponent type recurrence internal) recur-rule->rrule-sxml) v)))

;; sepparate since this text shouldn't be escaped
(define (write-text _ v)
  ;; TODO out type should be xsd:string.
  ;; Look into what that means, and escape
  ;; from there
  ((xml xcal 'text) v))



(define sxml-writers (make-hash-table))
(for simple-type in '(BINARY DURATION CAL-ADDRESS DURATION FLOAT INTEGER
                             #| TODO PERIOD |# URI UTC-OFFSET)
     (hashq-set! sxml-writers simple-type
                 (lambda (p v)
                   ((xml xcal (downcase-symbol simple-type))
                    (((@ (vcomponent formats ical types) get-writer) simple-type)
                     p v)))))

(hashq-set! sxml-writers 'BOOLEAN write-boolean)
(hashq-set! sxml-writers 'DATE write-date)
(hashq-set! sxml-writers 'DATE-TIME write-datetime)
(hashq-set! sxml-writers 'TIME write-time)
(hashq-set! sxml-writers 'RECUR write-recur)
(hashq-set! sxml-writers 'TEXT write-text)

(define (get-writer type)
  (or (hashq-ref sxml-writers type #f)
      (error (G_ "No writer for type") type)))

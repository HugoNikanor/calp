(define-module (vcomponent create)
  :use-module (vcomponent base)
  :use-module ((srfi srfi-1) :select (last drop-right car+cdr))
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((ice-9 hash-table) :select (alist->hashq-table))
  :use-module ((hnh util) :select (kvlist->assq ->))
  :export (with-parameters
           as-list
           vcomponent
           vcalendar vevent
           vtimezone standard daylight
           ))

;; TODO allow parameters and list values at same time



;; Convert a scheme keyword to a symbol suitable for us
(define (keyword->key keyword)
  (-> keyword
      keyword->string
      string-upcase
      string->symbol))

(define (symbol-upcase symbol)
  (-> symbol
      symbol->string
      string-upcase
      string->symbol))

;; Upcase the keys in an association list. Keys must be symbols.
(define (upcase-keys alist)
  (map (lambda (pair) (cons (symbol-upcase (car pair))
                       (cdr pair)))
       alist))



(define-immutable-record-type <almost-vline>
  (make-almost-vline parameters value)
  almost-vline?
  (parameters almost-vline-parameters)
  (value almost-vline-value))

(define (almost-vline->vline key almost-vline)
  (make-vline key
              (almost-vline-value almost-vline)
              (almost-vline-parameters almost-vline)))

(define (with-parameters . args*)
  (define parameters (drop-right args* 1))
  (define value (last args*))
  (make-almost-vline
   (-> parameters
       kvlist->assq
       upcase-keys
       alist->hashq-table)
   value))



(define-immutable-record-type <list-value>
  (make-list-value value)
  list-value?
  (value list-value-value))

(define (as-list arg)
  (make-list-value arg))



(define (vcomponent type . attrs*)
  (define component (make-vcomponent type))
  (define attrs*-len (length attrs*))
  (unless (zero? attrs*-len)
    (let ((attrs children
                 (if (and (list? (list-ref attrs* (- attrs*-len 1)))
                          (or (= 1 attrs*-len)
                              (not (keyword? (list-ref attrs* (- attrs*-len 2))))))
                     (values (drop-right attrs* 1)
                             (last attrs*))
                     (values attrs* '()))))
      (for-each (lambda (pair)
                  (let ((key value (car+cdr pair)))
                    (cond
                     ((almost-vline? value)
                      (set! (prop* component key)
                        (almost-vline->vline key value)))
                     ((list-value? value)
                      (set! (prop* component key)
                        (map (lambda (value)
                               (make-vline key value (make-hash-table)))
                             (list-value-value value))))
                     (else
                      (set! (prop component key) value)))))
                (upcase-keys (kvlist->assq attrs)))

      ;; Attach children
      (for-each (lambda (child) (add-child! component child))
                children)))

  component)

(define (vcalendar . attrs)
  (apply vcomponent 'VCALENDAR attrs))

(define (vevent . attrs)
  (apply vcomponent 'VEVENT attrs))

(define (vtimezone . attrs)
  (apply vcomponent 'VTIMEZONE attrs))

(define (standard . attrs)
  (apply vcomponent 'STANDARD attrs))

(define (daylight . attrs)
  (apply vcomponent 'DAYLIGHT attrs))

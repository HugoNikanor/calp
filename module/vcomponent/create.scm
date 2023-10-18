(define-module (vcomponent create)
  :use-module ((vcomponent base) :prefix vcs-)
  :use-module ((vcomponent base)
               :select (vline key add-child prop* vline?))
  :use-module ((srfi srfi-1) :select (fold last drop-right car+cdr))
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((hnh util table) :select (alist->table))
  :use-module ((hnh util) :select (swap init+last kvlist->assq ->))
  :use-module (hnh util object)
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



(define (with-parameters . args)
  (define-values (parameters value)
    (init+last args))
  (vline
   key: 'PLACEHOLDER
   vline-value: value
   vline-parameters:
   (-> parameters
       kvlist->assq
       upcase-keys
       alist->table)))



(define-type (list-value)
  (list-value-value))

(define (as-list arg)
  (list-value list-value-value: arg))



(define (vcomponent type . attrs*)
  (define-values (attrs children)
    (cond ((null? attrs*)          (values '() '()))
          ((even? (length attrs*)) (values attrs* '()))
          (else                    (init+last attrs*))))
  ;; TODO add-child requires a UID on the child
  ;; Possibly just genenerate one here if missing
  (fold (swap add-child)
        (fold (lambda (pair component)
                (let ((k value (car+cdr pair)))
                  (prop* component k
                         (cond ((vline? value)
                                (key value k))
                               ((list-value? value)
                                (map (lambda (value) (vline key: k vline-value: value))
                                     (list-value-value value)))
                               (else (vline key: k vline-value: value))))))
              (vcs-vcomponent
               type: type)
              (upcase-keys (kvlist->assq attrs)))
        children))

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

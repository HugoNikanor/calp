(define-module (vcomponent base)
  :use-module (hnh util)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module (srfi srfi-88)
  :use-module ((hnh util type) :select (false?))
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (hnh util table)
  :use-module (hnh util uuid)
  :use-module ((hnh util exceptions) :select (unreachable))
  :export (vline
           vline?
           vline-value
           key
           vline-parameters
           vline-source

           vcomponent
           vcomponent?
           children type parent
           add-child

           vcomponent-equal?

           remove-property
           prop* prop
           extract extract*

           set-properties

           ;; value
           param

           parameters
           properties

           x-property?
           internal-field?
           )
  )



;;; <vcomponent>
;;;   <properties>
;;;     <dtstart>
;;;       <parameters>
;;;         <tzid><text>Europe/Stockholm</text></tzid>
;;;       </parameters>
;;;       2020-01-01T13:37:50
;;;     </dtstart>
;;;   </properties>
;;; </vcomponent>
;;;

;;; TODO at least when called from serialize-vcomponent, this should
;;; emit something which allows the serialized vcomponent to be
;;; fed back into the parser to get the object back.
(define (print-vline v p)
  ((@ (ice-9 pretty-print) pretty-print)
   `(vline key: ,(key v)
           vline-value: ,(vline-value v)
           ,@(let ((params (table->list (vline-parameters v))))
               (if (null? params)
                   '()
                   `(vline-parameters:
                     ,(concatenate (for (key . value) in params
                                        `(,(symbol->keyword key)
                                          ,value)))))))
   p))

(define-type (vline printer: print-vline)
  ;; TODO why does vline contain its own key?
  (key type: symbol?)
  (vline-value)
  (vline-parameters default: (table) type: table?)
  (vline-source default: "" type: string?))

(define (vline-equal? a b)
  (and (eq? (key a) (key b))
       (equal? (vline-value a)
               (vline-value b))
       (equal? (table->list (vline-parameters a))
               (table->list (vline-parameters b)))))

(define (serialize-vline line)
  (let ((parameters
         (table->list (vline-parameters line))))
    (if (null? parameters)
        (vline-value line)
        `(with-parameters
          ,@(concatenate (for (key . value) in parameters
                              `(,(symbol->keyword key)
                                ,value)))
          ,(vline-value line)))))

(define (serialize-vcomponent c)
  (let ((children (table->list (vcomponent-children c))))
    `(vcomponent ',(type c)
                 ,@(concatenate
                    (for (key . value) in (table->list (component-properties c))
                         (list (-> key symbol->string
                                   string-downcase
                                   string->keyword)
                               (cond ((list? value)
                                      `(as-list (list
                                                 ,@(map serialize-vline value))))
                                     ((vline? value) (serialize-vline value))
                                     (else (unreachable
                                            "serialize-vcomponent"
                                            "Expected vline or list of vline, got ~s"
                                            value))))))
                 ,@(unless (null? children)
                     `((list ,@(map (lambda (child) (serialize-vcomponent child))
                                    (map cdr children))))))))

(define (print-vcomponent c p)
  ((@ (ice-9 pretty-print) pretty-print)
   (serialize-vcomponent c)
   p))

(define-type (vcomponent printer: print-vcomponent)
  (type                        type: symbol?)
  (vcomponent-children
              default: (table) type: table?)
  (component-properties
              default: (table) type: table?)
  (parent     default: #f      type: (or false? vcomponent?)))

(define (vcomponent-equal? a b)
  (and (eqv? (type a) (type b))
       (= (length (children a)) (length (children b)))
       (every vcomponent-equal?
            (sort* (children a) string< (extract 'UID))
            (sort* (children b) string< (extract 'UID)))
       (every (lambda (a b)
                (and (eq? (car a) (car b))
                     (cond ((and (list? (cadr a))
                                 (list? (cadr b)))
                            (every vline-equal?
                                   (cadr a)
                                   (cadr b)))
                           ((and (not (list? (cadr a)))
                                 (not (list? (cadr b))))
                            (vline-equal? (cadr a)
                                          (cadr b)))
                           (else #f))))
              (properties a) (properties b))))

;; Accessor to whole vline
(define prop*
  (case-lambda
    ((object key)
     (table-get (component-properties object) key))
    ((object key value)
     (component-properties object
      (table-put (component-properties object) key value)))))

;; Lens focusing the given property in the object.
(define (prop% k) (lens-compose component-properties* (table-focus k)))


(define (children c)
  (map cdr (table->list (vcomponent-children c))))

(define (add-child parent* child)
  (modify parent* vcomponent-children*
          (lambda (table)
            (let ((child
                   ;; TODO is this the correct place to generate UIDs?
                   (if (prop child 'UID)
                       child
                       (prop child 'UID (uuid)))))
              (table-put table
                         (as-symb (prop child 'UID))
                         (parent child parent*))))))



;; (define prop (compose-lens vline-value prop*))
(define prop
  (case-lambda
    ((comp key) (and=> (prop* comp key)
                       (lambda (x)
                         (if (list? x)
                             (map vline-value x)
                             (vline-value x)))))
    ((comp k v)
     (cond ((prop* comp k)
            => (lambda (vline)
                 (prop* comp k (vline-value vline v))))
           (else
            (prop* comp k (vline key: k vline-value: v)))))))

(define (remove-property component key)
  (modify component component-properties*
          (lambda (props) (table-remove props key))))

;;; TODO where is remove-child?

(define param
  ;; TODO list?
  (case-lambda ((vline key) (and=> (table-get (vline-parameters vline) key) list))
               ((vline k v) (vline-parameters
                             vline
                             (table-put (vline-parameters vline) k v)))))



;; Returns the parameters of a property as an assoc list.
;; @code{(map car <>)} leads to available parameters.
(define (parameters vline)
  (map (compose list car+cdr)
       (table->list (vline-parameters vline))))

(define (properties component)
  (map (compose list car+cdr)
       (table->list (component-properties component))))

(define (extract field)
  (lambda (e) (prop e field)))

(define (extract* field)
  (lambda (e) (prop* e field)))

(define (x-property? symb)
  (string=? "X-" (string-take (symbol->string symb) 2)))

(define* (internal-field? symbol optional: (prefix "-"))
  (string=? prefix
            (string-take-to (symbol->string symbol)
                            (string-length prefix))))


(define (set-properties component . pairs)
  ;; (format (current-error-port) "component: ~s, pairs: ~s~%" component pairs)
  (fold (lambda (pair component) (prop component (car pair) (cdr pair)))
        component
        pairs))

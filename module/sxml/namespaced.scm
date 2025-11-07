(define-module (sxml namespaced)
  :use-module (sxml ssax)
  :use-module (sxml util)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util state-monad)
  :use-module ((hnh util io) :select (->port))
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (hnh util table)
  :use-module ((hnh util lens) :select (car* cdr* modify lens-compose))
  :export (xml->namespaced-sxml
           namespaced-sxml->xml
           namespaced-sxml->sxml
           namespaced-sxml->sxml/namespaces
           sxml->namespaced-sxml

           xml
           attribute

           xml-element
           xml-element?
           xml-element*?                ; TODO why is this exported?
           xml-element-tagname    xml-element-tagname*
           xml-element-namespace  xml-element-namespace*
           xml-element-attributes xml-element-attributes*
           xml-element-children   xml-element-children*

           xml-document
           xml-document-root  xml-document-root*
           xml-document-pis   xml-document-pis*

           pi-element
           pi-element?
           pi-tag  pi-tag*
           pi-body pi-body*
           ))


(define (car+cadr p) (values (car p) (cadr p)))
(define (2list->pair l)
  (call-with-values (lambda () (car+cadr l)) cons))


;; XML processing instruction elements (and other things with identical syntax)
;; For example: <?xml version="1.0" encoding="utf-8"?> would be encoded as
;; (make-pi-element 'xml "version=\"1.0\" encoding=\"utf-8\"")
;; tag should always be a symbol
;; body should always be a string
(define-type (pi-element
              constructor: (lambda (constructor typecheck)
                             (lambda (tag body)
                               (typecheck tag body)
                               (constructor tag body)))
              serializer: (lambda (o) `(pi-element
                                   ,(serialize (pi-tag o))
                                   ,(serialize (pi-body o)))))
  (pi-tag  type: symbol?)
  (pi-body type: string?))


(define (serialize-xml-element object)
  `((xml ,@(awhen (xml-element-namespace object) (list (serialize it)))
         ,(serialize (xml-element-tagname object))
         ,@(let ((attrs (xml-element-attributes object)))
             (if (table-empty? attrs)
                 '()
                 (list (serialize (table->list attrs))))))
    ,@(map serialize (xml-element-children object))))

(define-type (xml-element serializer: serialize-xml-element)
  (xml-element-tagname    type: symbol?
                          keyword: tag)
  (xml-element-namespace  type: (or false? symbol?)
                          default: #f
                          keyword: ns)
  (xml-element-attributes type: table?
                          ;; (table-of symbol? string?)
                          ;; (table-of symbol? any?)
                          default: (table)
                          keyword: attributes)
  (xml-element-children   type: (list-of (or string?
                                             xml-element?
                                             pi-element?))
                          keyword: children
                          default: '()))


;;; NOTE Due to how define-type works, and how guile expands forms,
;;; this MUST be placed after the xml-element declaration. Otherwise
;;; `xml-element?' is of type syntax-transformer, which isn't a procedure.
(define (xml-element*? x)
  (xml-element? x))


(define-type (xml-document)
  (xml-document-root type: xml-element*?
                     keyword: root)
  (xml-document-pis  type: (list-of pi-element?)
                     keyword: pi
                     default: '()))


(define xml
  (case-lambda
    ((tag)          (lambda children (xml-element children: children tag: tag)))
    ((ns tag)       (lambda children (xml-element children: children tag: tag ns: ns)))
    ((ns tag attrs) (lambda children (xml-element children: children tag: tag ns: ns
                                             attributes: (alist->table attrs))))))

(define (attribute xml attr)
  (table-get (xml-element-attributes xml) attr))

(define (add-child child el)
  (xml-element-children el (cons child (xml-element-children el))))

(define (make-stack) (list))

(define push cons)
(define peek car)
(define pop car+cdr)

;;; Lens focusing the first element of list matching predicate
(define (find* predicate)
  (lambda (list)
    (lambda (f)
      (let loop ((rem list))
        (cond ((null? rem) '())
              ((predicate (car rem))
               (cons (f (car rem))
                     (cdr rem)))
              (else (cons (car rem)
                          (loop (cdr rem)))))))))

(define* (parser key: trim-whitespace?)
  (ssax:make-parser

   ;; DOCTYPE
   ;; (lambda (port docname systemid internal-subset? seed)
   ;;   (format (current-error-port)
   ;;           "doctype: port=~s, docname=~s, systemid=~s, internal-subset?=~s, seed=~s~%"
   ;;           port docname systemid internal-subset? seed)
   ;;   (values #f '() '() seed))

   ;; UNDECL-ROOT
   ;; (lambda (elem-gi seed)
   ;;   (format (current-error-port) "Undecl-root: ~s~%" elem-gi)
   ;;   (values #f '() '() seed))

   ;; DECL-ROOT
   ;; (lambda (elem-gi seed)
   ;;   (format (current-error-port) "Decl-root: ~s~%" elem-gi)
   ;;   seed)

   NEW-LEVEL-SEED
   (lambda (elem-gi attrs namespaces expected-content seed)
     ;; TODO attribute keys are either plain symbols, or
     ;; a pair of namespace and symbol
     (push
      (match elem-gi
        ((ns . tag) (xml-element tag: tag attributes: (alist->table attrs) ns: ns))
        (tag        (xml-element tag: tag attributes: (alist->table attrs))))
      seed))

   FINISH-ELEMENT
   (lambda (elem-gi attributes namespaces parent-seed seed)
     (let ((head tail (pop seed)))
       (modify tail
               (find* xml-element*?)
               (lambda (parent)
                 (add-child
                  (modify head xml-element-children* reverse)
                  parent)))))

   CHAR-DATA-HANDLER
   (lambda (s1 s2 seed)
     (define s
       (if trim-whitespace?
           (string-trim-both (string-append s1 s2))
           (string-append s1 s2)))
     (if (string-null? s)
         seed
         (modify seed (lens-compose (find* xml-element*?)
                                    xml-element-children*)
                 (lambda (ch) (cons s ch)))))


   PI
   ((*DEFAULT* . (lambda (port pi-tag seed)
                   (let ((body (ssax:read-pi-body-as-string port)))
                     (modify seed
                             (find* xml-element*?)
                             (lambda (parent)
                               (add-child (pi-element pi-tag body)
                                          parent)))))))))


(define* (xml->namespaced-sxml port-or-string key: (trim-whitespace? #t))
  (define result
    (with-ssax-error-to-port
     (current-error-port)
     (lambda () ((parser trim-whitespace?: trim-whitespace?)
                 (->port port-or-string)
                 (list ((xml 'ROOT)))))))

  (let ((roots pis
               (partition xml-element*?
                          (-> result peek xml-element-children))))
    (xml-document
     root: (car roots)
     pi: (reverse pis))))

(define (pi-element->sxml pi)
  `(*PI* ,(pi-tag pi) ,(pi-body pi)))



(define (ns-pair->attribute pair)
  (let ((fqdn short (car+cdr pair)))
    (list (string->symbol (format #f "xmlns:~a" short))
          (symbol->string fqdn))))

;; Takes an association list from full namespace names (as symbols), to their
;; short forms, and returns a list containing xmlns:x-attributes suitable for
;; splicing into scheme's "regular" sxml.
(define (ns-alist->attributes ns)
  (map ns-pair->attribute ns))



(define (get-prefix ns)
  (do namespaces <- (get)
      (cond ((assq-ref namespaces ns) => return)
            (else (do prefix = (gensym "ns")
                      (put (acons ns prefix namespaces))
                      (return prefix))))))


(define (xml-element->sxml el)
  (do tag <- (cond ((xml-element-namespace el)
                    => (lambda (ns)
                         (do pre <- (get-prefix ns)
                             (return
                              (string->symbol
                               (format #f "~a:~a" pre (xml-element-tagname el)))))))
                   (else (return (xml-element-tagname el))))
      children <- (sequence (map namespaced-sxml->sxml*
                                 (xml-element-children el)))
      (return
       (cond ((table-empty? (xml-element-attributes el))
              `(,tag ,@children))
             (else
              `(,tag (@ ,@(map (lambda (p) (call-with-values (lambda () (car+cdr p)) list))
                               (table->list
                                (xml-element-attributes el))))
                     ,@children))))))

(define (sxml->xml-element el namespaces)
  (lambda (children)
    (let ((tag-symb attrs
                    (match el
                      ((tag ('@ attrs ...))
                       (values tag (map (lambda (p) (apply cons p)) attrs)))
                      ((tag) (values tag '())))))
      (let ((parts (string-split (symbol->string tag-symb) #\:)))
        (cons (case (length parts)
                ((1) (xml (assoc-ref namespaces #f)
                          (string->symbol (car parts)) attrs))
                ((2)
                 (cond ((assoc-ref namespaces (string->symbol (car parts)))
                        => (lambda (ns) (xml ns (string->symbol (cadr parts)) attrs)))
                       (else (scm-error 'missing-namespace "sxml->xml-element"
                                        "Unknown namespace prefix encountered: ~s (on tag ~s)"
                                        (list (car parts) (cadr parts))
                                        #f))))
                (else (scm-error 'misc-error "sxml->xml-element"
                                 "Invalid QName: more than one colon ~s"
                                 (list tag-symb) #f)))
              children)))))


(define (namespaced-sxml->sxml* tree)
  (cond ((string? tree)      (return tree))
        ((xml-element*? tree) (xml-element->sxml tree))
        ((pi-element? tree)  (return (pi-element->sxml tree)))
        ((xml-document? tree)
         (do pis <- (sequence (map namespaced-sxml->sxml*
                                   (xml-document-pis tree)))
             el  <- (namespaced-sxml->sxml*
                    (xml-document-root tree))
             (return `(*TOP* ,@pis ,el))))
        (else (scm-error 'misc-error "namespaced-sxml->sxml*"
                         "Unexpected token in tree: ~s"
                         (list tree)
                         #f))))


;; Takes a tree of namespaced-sxml, and optionally an assoc list from namespace symbols, to prefered prefix.
;; Returns a sxml tree, with xmlns:<prefix>=namespace attributes
(define* (namespaced-sxml->sxml tree optional: (namespace-prefixes '()))
  (let ((tree ns ((namespaced-sxml->sxml* tree) namespace-prefixes)))
    (modify-root-element
     tree
     (lambda (root)
       ;; TODO only include namespaces actually used in document
       (add-attributes root (ns-alist->attributes ns))))))

(define* (namespaced-sxml->xml tree key:
                               (namespaces '())
                               (port (current-output-port)))
  ((@ (sxml simple) sxml->xml)
   (namespaced-sxml->sxml tree namespaces) port))

;; Takes a tree of namespaced-sxml, and optionally an assoc list from namespace symbols, to prefered prefix.
;; Returns two values: a sxml tree without declared namespaces
;;      and a association list from namespace symbols, to used prefixes
(define* (namespaced-sxml->sxml/namespaces tree optional: (namespace-prefixes '()))
  ((namespaced-sxml->sxml* tree) namespace-prefixes))




(define (split-namespace symb)
  (apply values (map string->symbol
                     (string-split (symbol->string symb) #\:))))

;; Takes an sxml tree, and an association list from prefixes to namespaces
;; Returns a namespaced sxml tree
(define (sxml->namespaced-sxml tree namespaces)
  (match tree
    (('*PI* tag body) (pi-element tag body))
    (('*TOP* rest ...)
     (let ((groups
            (group-by (lambda (x)
                        (cond ((xml-element*? x) 'el)
                              ((pi-element?  x) 'pi)
                              (else #f)))
                      (map (lambda (r) (sxml->namespaced-sxml r namespaces))
                           rest))))
       ;; NOTE should multiple roots be allowed? sxml->xml allows it.
       ;; NOTE should a warning be emitted if the #f groups isn't empty?
       (xml-document root: (car (assoc-ref groups 'el))
                     pi:   (or (assoc-ref groups 'pi) '()))))

    ((el ('@ attrs ...) rest ...)
     (apply (call-with-values (lambda () (split-namespace el))
              (case-lambda ((tag)
                            (xml (assoc-ref namespaces #f)
                                 tag attrs))
                           ((ns tag)
                            (cond ((assoc-ref namespaces ns)
                                   => (lambda (ns) (xml ns tag attrs)))
                                  (else (scm-error
                                         'missing-namespace
                                         "sxml->xml-element"
                                         "Unknown namespace prefix encountered: ~s (on tag ~s)"
                                         (list ns tag)
                                         #f))))))
            (map (lambda (el) (sxml->namespaced-sxml el namespaces))
                 rest)))

    ((el rest ...)
     (sxml->namespaced-sxml `(,el (@) ,@rest) namespaces))

    (atom atom)))

;;; TODO read intro-comment in SSAX file
;;; TODO Figure out how to still use (sxml match) and (sxml xpath) with these
;;;      new trees (probably rewriting to a "regular" sxml tree, and keeping
;;;      a strict mapping of namespaces)

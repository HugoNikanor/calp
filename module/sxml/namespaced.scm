(define-module (sxml namespaced)
  :use-module (sxml ssax)
  :use-module (sxml util)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util state-monad)
  :use-module ((hnh util io) :select (->port))
  :use-module (hnh util object)
  :use-module (hnh util type)
  :export (xml->namespaced-sxml
           namespaced-sxml->xml
           namespaced-sxml->sxml
           namespaced-sxml->sxml/namespaces
           sxml->namespaced-sxml
           xml
           attribute

           xml-element
           xml-element?
           xml-element-tagname
           xml-element-namespace
           xml-element-attributes

           pi-element
           pi-element?
           pi-tag
           pi-body
           ))

;; XML processing instruction elements (and other things with identical syntax)
;; For example: <?xml version="1.0" encoding="utf-8"?> would be encoded as
;; (make-pi-element 'xml "version=\"1.0\" encoding=\"utf-8\"")
;; tag should always be a symbol
;; body should always be a string
(define-type (pi-element
              constructor: (lambda (constructor typecheck)
                             (lambda (tag body)
                               (typecheck tag body)
                               (constructor tag body))))
  (pi-tag  type: symbol?)
  (pi-body type: string?))

(define-type (xml-element)
  (xml-element-tagname    type: symbol?
                          keyword: tag)
  (xml-element-namespace  type: (or false? symbol?)
                          default: #f
                          keyword: ns)
  (xml-element-attributes type: (list-of (pair-of symbol? string?))
                          default: '()
                          keyword: attributes))

(define xml
  (case-lambda
    ((tag) (xml-element tag: tag))
    ((ns tag) (xml-element tag: tag ns: ns))
    ((ns tag attrs) (xml-element tag: tag ns: ns attributes: attrs))))

(define (attribute xml attr)
  (assoc-ref (xml-element-attributes xml) attr))


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
  (lambda (elem-gi attributes namespaces expected-content seed)
    (cons
     (list
      (match elem-gi
        ((ns . tag) (xml-element tag: tag ns: ns attributes: attributes))
        (tag (xml-element tag: tag attributes: attributes))))
     seed))

  FINISH-ELEMENT
  (lambda (elem-gi attributes namespaces parent-seed seed)
    (match seed
      (((self . self-children) (parent . children) . rest)
       `((,parent (,self ,@(reverse self-children)) ,@children)
         ,@rest))))

  CHAR-DATA-HANDLER
  (lambda (str1 str2 seed)
    (define s
      (if trim-whitespace?
          (string-trim-both (string-append str1 str2))
          (string-append str1 str2)))
    (cond ((string-null? s) seed)
          (else
           (match seed
             (((parent . children) . rest)
              `((,parent ,(string-append str1 str2)
                         ,@children)
                ,@rest))))))

  PI
  ((*DEFAULT* . (lambda (port pi-tag seed)
                  (let ((body (ssax:read-pi-body-as-string port)))
                    (match seed
                      (((parent . children) . rest)
                       `((,parent ,(pi-element pi-tag body) ,@children)
                         ,@rest)))))))
  ))


(define* (xml->namespaced-sxml port-or-string key: (trim-whitespace? #t))
  (match (with-ssax-error-to-port
          (current-error-port)
          (lambda () ((parser trim-whitespace?: trim-whitespace?)
                 (->port port-or-string)
                 '((*TOP*)))))
    ((('*TOP* . items))
     `(*TOP* ,@(reverse items)))))

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
      (return
       (lambda (children)
         (cond ((null? (xml-element-attributes el))
                `(,tag ,@children))
               (else
                `(,tag (@ ,@(map (lambda (p)
                                   (call-with-values (lambda () (car+cdr p)) list))
                                 (xml-element-attributes el)))
                       ,@children)))))))

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
  (cond ((null? tree)   (return tree))
        ((string? tree) (return tree))
        ((pi-element? tree) (return (pi-element->sxml tree)))
        ((not (pair? tree)) (return tree))
        ((car tree) symbol?
         => (lambda (symb)
              (case symb
                ((*TOP*) (do children <- (sequence (map namespaced-sxml->sxml*
                                                        (cdr tree)))

                             (return (cons '*TOP* children))))
                (else (return tree)))))
        ((xml-element? (car tree))
         (do proc <- (xml-element->sxml (car tree))
             children <- (sequence (map namespaced-sxml->sxml* (cdr tree)))
             (return (proc children))))

        ;; list of xml-element?
        (else (scm-error 'misc-error "namespaced-sxml->sxml*"
                         "Unexpected token in tree: ~s"
                         (list tree)
                         #f))))


;; Takes a tree of namespaced-sxml, and optionally an assoc list from namespace symbols, to prefered prefix.
;; Returns a sxml tree, with xmlns:<prefix>=namespace attributes
(define* (namespaced-sxml->sxml tree optional: (namespace-prefixes '()))
  (let ((tree ns ((namespaced-sxml->sxml* tree) namespace-prefixes)))
    ((get-root-element tree)
     (lambda (root)
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

;; Takes an sxml tree, and an association list from prefixes to namespaces
;; Returns a namespaced sxml tree
(define (sxml->namespaced-sxml tree namespaces)
  (match tree
    (('*PI* tag body) (pi-element tag body))
    (('*TOP* rest ...)
     `(*TOP* ,@(map (lambda (r) (sxml->namespaced-sxml r namespaces))
                    rest)))
    ((el ('@ attrs ...) rest ...)
     ((sxml->xml-element `(,el (@ ,@attrs)) namespaces)
      (map (lambda (el) (sxml->namespaced-sxml el namespaces))
           rest)))
    ((el rest ...)
     ((sxml->xml-element `(,el) namespaces)
      (map (lambda (el) (sxml->namespaced-sxml el namespaces))
           rest)))
    (atom atom)))

;;; TODO read intro-comment in SSAX file
;;; TODO Figure out how to still use (sxml match) and (sxml xpath) with these
;;;      new trees (probably rewriting to a "regular" sxml tree, and keeping
;;;      a strict mapping of namespaces)


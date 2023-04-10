(define-module (test sxml-namespaced)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (ice-9 match)
  :use-module (sxml namespaced)
  :use-module (hnh util state-monad)
  )

;;; TODO tests with attributes

(define (ns x)
  (string->symbol (format #f "http://example.com/~a" x)))

(define (namespaced-symbol ns symb)
  (string->symbol (format #f "~a:~a" ns symb)))



(test-group "XML constructor utility procedure"
  (test-equal "3 args"
    (make-xml-element 'tagname 'namespace 'attributes)
    (xml 'namespace 'tagname 'attributes))

  (test-equal "2 args"
    (make-xml-element 'tagname 'namespace '())
    (xml 'namespace 'tagname))

  (test-equal "1 args"
    (make-xml-element 'tagname #f '())
    (xml 'tagname)))



(test-group "xml->namespaced-sxml"

  (test-equal
      `(*TOP* (,(xml 'tag)))
    (xml->namespaced-sxml "<tag/>"))

  (test-equal
      `(*TOP* (,(xml 'ns1 'tag)))
    (xml->namespaced-sxml "<tag xmlns='ns1'/>"))

  (test-equal
      `(*TOP* (,(xml 'ns2 'tag)))
    (xml->namespaced-sxml "<x:tag xmlns='ns1' xmlns:x='ns2'/>"))

  (test-equal
      `(*TOP* (,(xml 'ns2 'tag)
               (,(xml 'ns1 'tag))))
    (xml->namespaced-sxml "<x:tag xmlns='ns1' xmlns:x='ns2'><tag/></x:tag>"))

  (test-equal "PI are passed directly"
      `(*TOP* ,(make-pi-element 'xml "encoding=\"utf-8\" version=\"1.0\"")
              (,(xml 'tag)))
      (xml->namespaced-sxml "<?xml encoding=\"utf-8\" version=\"1.0\"?><tag/>"))

  (test-equal "Document with whitespace in it"
    `(*TOP* ,(make-pi-element 'xml "")
            (,(xml 'root)
             " "
             (,(xml 'a))
             ))
    (xml->namespaced-sxml "<?xml?><root> <a/></root>"
                          trim-whitespace?: #f))

  ;; TODO is this expected? xml->sxml discards it.
  (test-equal "Whitespace before root is kept"
    `(*TOP* ,(make-pi-element 'xml "")
            (,(xml 'root)))
    (xml->namespaced-sxml "<?xml?> <root/>")))



;;; NOTE that sxml->namespaced-sxml currently ignores any existing xmlns
;;; attributes, since xml->sxml doesn't have those.
(test-group "sxml->namespaced-sxml"
  (test-equal "Simplest"
    `(,(xml 'a)) (sxml->namespaced-sxml '(a) '()))
  (test-equal "With *TOP*"
    `(*TOP* (,(xml 'a))) (sxml->namespaced-sxml '(*TOP* (a)) '()))
  (test-equal "Simplest with namespace"
    `(,(xml (ns 1) 'a))
    (sxml->namespaced-sxml '(x:a)
                           `((x . ,(ns 1)))))
  (test-equal "With pi"
    `(*TOP* ,(make-pi-element 'xml "test")
            (,(xml 'a)))
    (sxml->namespaced-sxml
     `(*TOP*
       (*PI* xml "test")
       (a))
     '()))
  (test-error "With unknown namespace"
    'missing-namespace
    (sxml->namespaced-sxml '(x:a) '())))



(test-group "namespaced-sxml->*"

  ;; /namespaces is the most "primitive" one
  (test-group "/namespaces"
   (test-group "Without namespaces"
       (call-with-values
           (lambda ()
             (namespaced-sxml->sxml/namespaces
              `(*TOP*
                (,(xml 'a)))))
         (lambda (tree namespaces)
           (test-equal `(*TOP* (a)) tree)
           (test-equal '() namespaces))))

   (test-group "With namespaces"
     (call-with-values
         (lambda ()
           (namespaced-sxml->sxml/namespaces
            `(*TOP*
              (,(xml (ns 1) 'a)
               (,(xml (ns 2) 'a))
               (,(xml 'a))))))
       (lambda (tree nss)
         (test-eqv 2 (length nss))
         (test-equal
             `(*TOP*
               (,(namespaced-symbol (assoc-ref nss (ns 1)) 'a)
                (,(namespaced-symbol (assoc-ref nss (ns 2)) 'a))
                (a)))
           tree))))

   (test-group "*PI*"
     (call-with-values
         (lambda ()
           (namespaced-sxml->sxml/namespaces
            `(*TOP*
              ,(make-pi-element 'xml "test")
              (,(xml 'a)))))
       (lambda (tree namespaces)
         (test-equal '() namespaces)
         (test-equal `(*TOP* (*PI* xml "test")
                             (a))
           tree)))))

  (test-group "namespaced-sxml->sxml"
    (test-equal "Without namespaces"
      '(*TOP* (a (@)))
      (namespaced-sxml->sxml `(*TOP* (,(xml 'a)))))

    (test-group "With namespaces"
     (match (namespaced-sxml->sxml `(*TOP* (,(xml (ns 1) 'a))))
       ;; (ns 1) hard coded to work with match
       (`(*TOP* (,el (@ (,key "http://example.com/1"))))
        (let ((el-pair  (string-split (symbol->string el) #\:))
              (key-pair (string-split (symbol->string key) #\:)))
          (test-equal "a" (cadr el-pair))
          (test-equal "xmlns" (car key-pair))
          (test-equal (car el-pair) (cadr key-pair))))
       (any
        (test-assert (format #f "Match failed: ~s" any) #f))))))

;; (namespaced-sxml->xml)
;; Literal strings


(test-error "Namespaces x is missing, note error"
  'parser-error
  (xml->namespaced-sxml "<x:a xmlns:y=\"http://example.com/1\"><x:b/></x:a>"
                        ; `((x . ,(ns 1)))
                        ))

(define-module (test sxml-namespaced-util)
  :use-module (srfi srfi-64)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util))

(define ns (gensym "xmlns-"))

(test-equal "XML Hash key with namespace"
  (string->symbol (format #f "~a:~a" ns 'a))
  (xml-element-hash-key ((xml ns 'a))))

(test-equal "XML Hash key without namespace"
  (string->symbol "#f:a")
  (xml-element-hash-key ((xml 'a))))

(test-group "Find element"
  (let ((el ((xml ns 'a))))
    (test-eq "Found element is the source element"
      el
      (find-child
       el
       (xml-element-children
        ((xml ns 'b)
         el ((xml ns 'a)))))))
  ;; TODO Test "find" failure
  )

(test-group "Element Match"
 (test-assert "Positive element match"
   (element-matches? ((xml 'a ns))
                     (list
                      ((xml 'a ns)
                       "Content here"))))
 (test-assert "Negative element match"
   (not (element-matches? ((xml 'a ns))
                          (list
                           ((xml 'b ns)
                            "Content here"))))))

'((sxml namespaced util))

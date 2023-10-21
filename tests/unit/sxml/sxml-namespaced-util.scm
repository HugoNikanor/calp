(define-module (test sxml-namespaced-util)
  :use-module (srfi srfi-64)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util))

(define ns (gensym "xmlns-"))

(test-equal "XML Hash key"
  (cons 'a ns)
  (xml-element-hash-key (xml 'a ns)))

(test-group "Find element"
  (let ((el `(,(xml ns 'a))))
    (test-eq "Found element is the source element"
      el
      (find-element
       (car el)
       `((,(xml ns 'b)) ,el (,(xml ns 'a))))))
  ;; TODO Test "find" failure
  )

(test-group "Element Match"
 (test-assert "Positive element match"
   (element-matches? (xml 'a ns)
                     (list (xml 'a ns)
                           "Content here")))
 (test-assert "Negative element match"
   (not (element-matches? (xml 'a ns)
                          (list (xml 'b ns)
                                "Content here")))))

(let ((el `(,(xml 'a) "Content")))
 (test-group "root-element"
   (test-equal "With PI"
     el (root-element `(*TOP* ,(pi-element 'doctype "HTML") ,el)))
   (test-equal "Without PI"
     el (root-element `(*TOP* ,el)))

   (test-equal "Bare"
     el (root-element el))))

'((sxml namespaced util))

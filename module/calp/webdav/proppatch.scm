(define-module (calp webdav proppatch)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp webdav property)
  :use-module (calp webdav resource)
  :use-module (sxml match)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((hnh util) :select (for))
  :use-module (hnh util type)
  :use-module ((calp namespaces) :select (webdav))
  :export (exec-propertyupdate)
  )


;; Returns a list of propstat objects
(define (exec-propertyupdate body resource)
   ;; Check that root is an xml element of type d:propertyupdate
   (typecheck body xml-element?)

   (unless (tag-matches? body 'propertyupdate webdav)
     (scm-error 'bad-request "exec-propertyupdate"
                "Root of PROPPATCH method must be a propertyupdate element, got ~s"
                (list (with-output-to-string
                        (lambda () (namespaced-sxml->xml (xml-element-children body '())))))
                '()))

   (define continuations
    (concatenate
     (for child in (xml-element-children body)
          ;; TODO while <d:set /> and <d:remove /> both MUST have a single <d:prop /> child, we should still explicitly check that, to allow better error messages
          (cond ((tag-matches? child 'set webdav)
                 ;; TODO handle xmllang correctly
                 (let ((prop-tag (find-child ((xml webdav 'prop))
                                             (xml-element-children child))))
                   (map (lambda (prop)
                          (cons (xml-element-children prop '())
                                (set-property!! resource prop)))
                        (filter xml-element? (xml-element-children prop-tag)))))

                ((tag-matches? child 'remove webdav)
                 (let ((prop-tag (find-child ((xml webdav 'prop))
                                             (xml-element-children child))))
                   (map (lambda (prop)
                          (cons (xml-element-children prop  '())
                                (remove-property!! resource prop)))
                        (filter xml-element? (xml-element-children prop-tag)))))

                (else '())
                ))))

   (merge-propstats
    (let loop ((continuations continuations))
      (if (null? continuations)
          '()
          (let ((tag proc (car+cdr (car continuations))))
            (catch #t (lambda ()
                        ;; This is expected to throw quite often
                        (proc)
                        (cons (propstat 200 (list tag))
                              (loop (cdr continuations))))
              (lambda err
                (cons (propstat 409 (list tag))
                      (mark-remaining-as-failed-dependency (cdr continuations))))))))))


(define (mark-remaining-as-failed-dependency pairs)
  (map (lambda (item)
         (propstat 424 (list (car item))))
       pairs))

(define-module (vcomponent formats xcal)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal))
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((hnh util) :select (->))
  :use-module ((calp namespaces) :select (xcal))
  :export (serialize deserialize))

(define* (serialize component port
                    key:
                    (namespaces `((,xcal . xcal)))
                    include-pis?)
  (namespaced-sxml->xml
   (xml-document
    pi: (if include-pis?
            (list (pi-element 'xml "version=\"1.0\" encoding=\"utf-8\"")
                  (pi-element 'xml-stylesheet "type=\"text/xsl\" href=\"xcal.xsl\""))
            (list))
    root: ((xml xcal 'icalendar) (vcomponent->sxcal component)))
   port: port
   namespaces: namespaces))

(define (serialize/object component)
  (call-with-output-string (lambda (p) (serialize component p))))


(define* (deserialize port)
  (-> port
      xml->namespaced-sxml
      xml-document-root

      ;; Remove containing icalendar
      xml-element-children car

      sxcal->vcomponent))

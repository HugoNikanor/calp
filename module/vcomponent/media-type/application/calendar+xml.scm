(define-module (vcomponent media-type application calendar+xml)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((vcomponent media-type application calendar+xml output)
               :select (vcomponent->sxcal))
  :use-module ((vcomponent media-type application calendar+xml parse)
               :select (sxcal->vcomponent))
  :use-module ((hnh util) :select (->))
  :use-module ((calp namespaces) :select (xcal))
  :use-module (vcomponent media-type)
  :use-module (oop goops)
  :export (format))

(define* (vcomponent->xml component port
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


(define* (xml->vcomponent port)
  (-> port
      xml->namespaced-sxml
      xml-document-root

      ;; Remove containing icalendar
      xml-element-children car

      sxcal->vcomponent))

(define format
  (calendar-data-format
   parser: xml->vcomponent
   serializer: vcomponent->xml))

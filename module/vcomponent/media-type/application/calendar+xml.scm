(define-module (vcomponent media-type application calendar+xml)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((vcomponent media-type application calendar+xml output)
               :select (vcomponent->sxcal serializers))
  :use-module ((vcomponent media-type application calendar+xml parse)
               :select (sxml->vcomponent parsers))
  :use-module ((hnh util) :select (->))
  :use-module ((calp namespaces) :select (xcal))
  :use-module (vcomponent media-type)
  :use-module (oop goops)
  :export (format)
  :re-export (parsers serializers))

(define (envelope body)
  (xml-document
   pi: (list (pi-element 'xml "version=\"1.0\" encoding=\"utf-8\"")
             ; (pi-element 'xml-stylesheet "type=\"text/xsl\" href=\"xcal.xsl\"")
             )
   root: ((xml xcal 'icalendar) body))  )

(define* (vcomponent->xml component port
                          key:
                          (envelope? #t)
                          (namespaces `((,xcal . xcal)))
                          )
  (namespaced-sxml->xml
   (if envelope?
       (envelope (vcomponent->sxcal component))
       (vcomponent->sxcal component))
   port: port
   namespaces: namespaces))


(define* (xml->vcomponent port)
  (-> port xml->namespaced-sxml sxml->vcomponent))

(define format
  (calendar-data-format
   parser: xml->vcomponent
   serializer: vcomponent->xml
   media-type: "application/calendar+xml"
   file-extension: "xcs"))

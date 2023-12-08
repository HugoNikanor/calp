(define-module (vcomponent formats xcal)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal ns-wrap))
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((hnh util) :select (->))
  :export (serialize deserialize))

(define-public xcal (string->symbol "urn:ietf:params:xml:ns:icalendar-2.0"))

(define* (serialize component port key: (namespaces `((,xcal . xcal))))
  (-> (vcomponent->sxcal component)
      ns-wrap
      (namespaced-sxml->xml port: port
                            namespaces: namespaces)))

(define (serialize/object component)
  (call-with-output-string (lambda (p) (serialize component p))))


(define* (deserialize port)
  (-> port
      xml->namespaced-sxml
      xml-document-root

      ;; Remove containing icalendar
      xml-element-children car

      sxcal->vcomponent))

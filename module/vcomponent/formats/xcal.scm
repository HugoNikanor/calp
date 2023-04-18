(define-module (vcomponent formats xcal)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal ns-wrap))
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((hnh util) :select (->))
  :export (serialize deserialize))


(define* (serialize component port key: (namespaces '()))
  (-> (vcomponent->sxcal component)
      ns-wrap
      (namespaced-sxml->xml port: port
                            namespaces: namespaces)))

(define (serialize/object component)
  (call-with-output-string (lambda (p) (serialize component p))))


(define* (deserialize port)
  (-> port
      xml->namespaced-sxml
      root-element                      ; Strip potential *TOP*
      cadr                              ; Remove containing icalendar
      sxcal->vcomponent))

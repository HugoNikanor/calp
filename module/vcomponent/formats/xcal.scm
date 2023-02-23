(define-module (vcomponent formats xcal)
  :use-module (sxml simple)
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal ns-wrap))
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((hnh util) :select (->))
  :export (serialize deserialize))


(define (serialize component port)
  (-> (vcomponent->sxcal component)
      ns-wrap
      (sxml->xml port)
      ))


(define (deserialize port)
  (-> (xml->sxml port)
      sxcal->vcomponent))

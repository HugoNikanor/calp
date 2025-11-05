(define-module (vcomponent media-type)
  :use-module (hnh util object)
  :export (calendar-data-format
           calendar-data-format?
           serializer
           parser))


(define-type (calendar-data-format)
  ;; ((parser <format>) <port>)
  parser
  ;; ((serializer <format>) vcomponent <port>)
  serializer)

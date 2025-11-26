(define-module (vcomponent media-type)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :export (calendar-data-format
           calendar-data-format?
           serializer
           parser
           media-type
           file-extension
           ))


(define-type (calendar-data-format)
  ;; ((parser <format>) <port>)
  parser
  ;; ((serializer <format>) vcomponent <port>)
  serializer
  ;; "text/calendar"
  (media-type type: (or string? false?))
  ;; "ics"
  (file-extension type: (or string? false?))
  )

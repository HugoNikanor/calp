(define-module (vcomponent media-type application calendar+json)
  :use-module (vcomponent media-type)
  :use-module (vcomponent media-type application calendar+json parse)
  :use-module (vcomponent media-type application calendar+json output)
  :export ((jcal-format . format))
  :re-export (serializers parsers))

(catch 'misc-error
  (lambda ()
    (use-modules (json))
    (provide 'formats-jcal))
  (lambda args 'no-op))

(define (json->vcomponent port)
  (parse/component (json->scm port)))

(define (vcomponent->json component port)
  (scm->json
   (serialize/object component)
   port))

(define jcal-format
  (calendar-data-format
   serializer: vcomponent->json
   parser: json->vcomponent
   media-type: "application/calendar+json"
   file-extension: "json"))

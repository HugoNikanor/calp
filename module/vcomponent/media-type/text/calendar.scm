(define-module (vcomponent media-type text calendar)
  :use-module ((vcomponent media-type text calendar output)
               :select (vcomponent->icalendar serializers))
  :use-module ((vcomponent media-type text calendar parse)
               :select (icalendar->vcomponent parsers))
  :use-module (vcomponent media-type)
  :export (format)
  :re-export (parsers serializers))

(define format
  (calendar-data-format
   parser: icalendar->vcomponent
   serializer: (lambda args (apply vcomponent->icalendar args))))

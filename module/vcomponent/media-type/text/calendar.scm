(define-module (vcomponent media-type text calendar)
  :use-module ((vcomponent media-type text calendar output)
               :select (vcomponent->icalendar))
  :use-module ((vcomponent media-type text calendar parse)
               :select (icalendar->vcomponent))
  :use-module (vcomponent media-type)
  :export (format))

(define format
  (calendar-data-format
   parser: icalendar->vcomponent
   serializer: vcomponent->icalendar))

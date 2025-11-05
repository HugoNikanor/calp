(define-module (vcomponent formats ical)
  :use-module ((vcomponent formats ical output)
               :select (vcomponent->icalendar))
  :use-module ((vcomponent formats ical parse)
               :select (icalendar->vcomponent))
  :use-module (vcomponent formats)
  :export (format))

(define format
  (calendar-data-format
   parser: icalendar->vcomponent
   serializer: vcomponent->icalendar))

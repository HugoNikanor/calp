(define-module (vcomponent geo)
  :use-module (hnh util)
  :use-module (srfi srfi-9 gnu)
  :export (make-geo geo-pos? geo-latitude geo-longitude))

(define-immutable-record-type <geographical-position>
  (make-geo latitude longitude)
  geo-pos?
  (latitude geo-latitude)
  (longitude geo-longitude))

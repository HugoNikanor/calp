(define-module (vcomponent geo)
  :use-module (util)
  :use-module (srfi srfi-9 gnu))

(define-immutable-record-type <geographical-position>
  (make-geo latitude longitude)
  geo-pos?
  (latitude geo-latitude)
  (longitude geo-longitude))

(export make-geo geo-pos? geo-latitude geo-longitude)

(define-module (vcomponent type geo)
  :use-module (hnh util object)
  :use-module (srfi srfi-88)
  :export (geo
           geo?
           geo-latitude  geo-longitude
           geo-latitude* geo-longitude*))

(define-type (geo)
  (geo-latitude  keyword: y type: number?)
  (geo-longitude keyword: x type: number?))


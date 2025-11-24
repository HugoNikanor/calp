;;; Commentary:
;;; Main "entry-point" to the iCalendar parser.
;;; Exports the venerable icalendar->vcomponent,
;;; and some configuration parameters for adding
;;; new types.
;;; Code:
(define-module (vcomponent media-type text calendar parse)
  :use-module ((ice-9 binary-ports) :select (get-bytevector-all))
  :use-module (vcomponent media-type text calendar parse-semantics)
  :use-module (vcomponent media-type text calendar parse-structure)
  :use-module (vcomponent media-type text calendar parse-types)
  :use-module (hnh util)
  :use-module (srfi srfi-88)
  :export (icalendar->vcomponent)
  :re-export (multi-valued-properties
              parsers
              get-parser
              ))


(define (parse-content-lines lst)
  (map parse-content-line lst))

(define* (icalendar->vcomponent port key: (encoding "UTF-8"))
  (-> (get-bytevector-all port)
      (bytevector->unfolded-lines encoding: encoding filename: (port-filename port))
      parse-content-lines
      assemble-vcomponent-tree))

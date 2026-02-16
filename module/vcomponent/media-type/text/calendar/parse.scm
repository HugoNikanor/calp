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

;;; TODO this fails with bad error messages on empty input
(define* (icalendar->vcomponent port key: (encoding "UTF-8"))
  (-> (get-bytevector-all port)
      (bytevector->unfolded-lines encoding: encoding filename: (port-filename port))


      ;; TODO look at optimizing the following, tested with a 12000 line file

      ;; 3.221425s real time, 12.446809s run time.  11.764367s spent in GC.
      parse-content-lines

      ;; 13.591375s real time, 67.736568s run time.  63.251476s spent in GC.
      assemble-vcomponent-tree))

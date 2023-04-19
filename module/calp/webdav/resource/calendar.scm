(define-module (calp webdav resource calendar)
  ;; :use-module (hnh util)
  ;; :use-module (datetime)
  ;; :use-module (sxml namespaced util)
  ;; :use-module (calp webdav property)
  ;; :use-module (ice-9 hash-table)
  :use-module (calp webdav resource calendar collection)
  :use-module (calp webdav resource calendar object)
  :export (
           calendar-resource?
)
  )

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(calp webdav resource calendar collection)))
(module-use! cm (resolve-interface '(calp webdav resource calendar object)))

(define (calendar-resource? x)
  (or (calendar-collection-resource? x)
      (calendar-object-resource? x)))








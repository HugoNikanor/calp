(define-module (calp webdav resource)
  :use-module (srfi srfi-88)
  :use-module (oop goops)
  :use-module (calp webdav resource base)
  :export ())

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(calp webdav resource base)))
(module-use! cm (resolve-interface '(calp webdav href)))


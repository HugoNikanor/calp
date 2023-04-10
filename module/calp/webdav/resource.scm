(define-module (calp webdav resource)
  :use-module (srfi srfi-88)
  :use-module (oop goops)
  :use-module (calp webdav resource base)
  :export (mount-resource!))

(define cm (module-public-interface (current-module)))
(module-use! cm (resolve-interface '(calp webdav resource base)))

;;; TODO mount-resource! vs add-child!
;;; Would a good idea be that add-resource! adds directly, and should
;;; be considered internal, while mount-resource! also runs post-add
;;; hooks, and could thereby be exported
(define-method (mount-resource! (this <resource>) (child <resource>))
  (add-child! this child))

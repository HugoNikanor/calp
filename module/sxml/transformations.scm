;;; Commentary:
;;; Module for transforming an already generated sxml tree.
;;; Ideally we would just generate the correct tree directly. But in some
;; circumstances that would lead to an absurd ammount of options and the
;; like, so these come in handy.
;;; Code:

(define-module (sxml transformations)
  :use-module (util)
  :use-module ((srfi srfi-1) :select (concatenate))
  :use-module ((sxml transform) :select (pre-post-order))
  )

;; sxml, bindings → sxml
(define-public (attribute-transformer
                tree attribute-bindings)

  (define bindings
    `((@ ,attribute-bindings
         . ,(lambda (_ . b) `(@ ,@b)))
      (*default* . ,(lambda (t . b) `(,t ,@b)))
      (*text* . ,(lambda (_ . b) (concatenate b)))))

  (pre-post-order tree bindings))


(define-public (href-transformer tree transformer)
  (attribute-transformer
   tree
   `((href . ,(lambda (_ . content)
                `(href ,@(transformer (string-concatenate content)))
                )))))

(define-public (href-prefixer tree prefix)
  (href-transformer
   tree (lambda (str) (string-append prefix str))))

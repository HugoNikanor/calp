(define-module (hnh util named-type)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (srfi srfi-88)
  :export (named-type
           (named-type-container? . named-type?)
           named-type-name
           named-type-type))


(define-syntax-rule (named-type expr)
  (named-type-container
   name: (quote expr)
   type: (lambda (x) (build-validator-body x expr))))


(define-type (named-type-container
              serializer: (lambda (t) `(named-type ,(named-type-name t))))
  (named-type-name keyword: name)
  (named-type-type keyword: type
                   type: procedure?))

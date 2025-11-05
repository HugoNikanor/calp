(define-module (vcomponent type utc-offset)
  :use-module (datetime)
  :use-module (srfi srfi-88)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :export (utc-offset
           utc-offset?
           offset-time offset-time*
           offset-direction offset-direction*

           utc-offset->string
           )
   )

(define-type (utc-offset)
  (offset-time keyword: offset type: time?)
  (offset-direction keyword: dir type: (memv '(+ -))))


(define (utc-offset->string offset time-fmt)
  (typecheck offset utc-offset?)
  (typecheck time-fmt string?)
  (string-append
   (symbol->string (offset-direction offset))
   (time->string time-fmt (offset-time offset))))

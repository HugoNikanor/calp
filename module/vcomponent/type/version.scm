(define-module (vcomponent type version)
  :use-module (hnh util object)
  :use-module ((hnh util type) :select (false?))
  :export (vcalendar-version
           vcalendar-version?
           version-min version-min*
           version-max version-max*
           vcalendar-version->string
           ))

(define-type (vcalendar-version)
  (version-min keyword: min
               type: (or false? string?)
               default: #f)
  (version-max keyword: max
               type: string?))


(define (vcalendar-version->string v)
  (string-append
   (cond ((version-min v) => (lambda (v) (format #f "~a;" v)))
         (else ""))
   (version-max v)) )

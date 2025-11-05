(define-module (vcomponent type version)
  :use-module (hnh util object)
  :use-module ((hnh util type) :select (false?))
  :export (vcalendar-version
           vcalendar-version?
           version-min version-min*
           version-max version-max*))

(define-type (vcalendar-version)
  (version-min keyword: min
               type: (or false? string?)
               default: #f)
  (version-max keyword: max
               type: string?))

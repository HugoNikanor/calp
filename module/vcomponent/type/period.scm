(define-module (vcomponent type period)
  :use-module (hnh util object)
  :use-module (vcomponent type duration)
  :use-module (datetime)
  :export (period
           period?
           period-start period-start*
           period-end period-end*)
  )

(define-type (period)
  (period-start type: datetime?
                keyword: start)
  (period-end type: (or datetime? duration?)
              keyword: end))

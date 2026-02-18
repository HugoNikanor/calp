(define-module (vcomponent type period)
  :use-module (hnh util)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (vcomponent type duration)
  :use-module (datetime)
  :export (period
           period?
           period-start period-start*
           period-end period-end*
           period->utc-datetimes
           ))

(define-type (period)
  (period-start type: datetime?
                keyword: start)
  (period-end type: (or datetime? duration?)
              keyword: end))


(define (period->utc-datetimes reference-zone period)
  (typecheck reference-zone string?)
  (typecheck period period?)
  (define zoned-start
    (modify (period-start period)
            tz* (lambda (tz) (or tz reference-zone))))
  (cond ((datetime? (period-end period))
         (values ((unval zone->utc) zoned-start)
                 ((unval zone->utc)
                  (modify (period-end period)
                          tz* (lambda (tz) (or tz reference-zone))))))
        ((duration? (period-end period))
         (values ((unval zone->utc) zoned-start)
                 ((unval zone->utc)
                  ;; TODO this won't work
                  (datetime+/zoneinfo zoned-start (period-end period)))))
        (else (scm-error
               'misc-error "period->utc-datetimes"
               "Invalid period passed, end is neither datetime or duration. Got: ~s"
               (list period) #f))))

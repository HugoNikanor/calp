;;; Commentary:
;;; Period of time.
;;; Note that this is limited to the iCalendar subset of the ISO 8601
;;; periods, meaning that all periods start with a datetime, and that
;;; iCalendar restrictions on durations apply (see duration module).
;;; Code:
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
  (define zone->utc1 (unval zone->utc))
  (values (zone->utc1 zoned-start)
          (cond ((zoned-datetime? (period-end period))
                 (zone->utc1 (period-end period)))
                ((unzoned-datetime? (period-end period))
                 (zone->utc1 (tz (period-end period)
                                 reference-zone)))
                ((duration? (period-end period))
                 (zone->utc1
                  (datetime+/zoneinfo zoned-start (period-end period))))
                (else (scm-error
                       'misc-error "period->utc-datetimes"
                       "Invalid period passed, end is neither datetime or duration. Got: ~s"
                       (list period) #f)))))

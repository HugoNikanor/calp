;;; Commentary:
;;; Many fields have very close formats in different media type, for
;;; example datetimes are "always" serialized as strings, with the
;;; same special handling of timezone ids.
;;; This module gathers all such common procedures.
;;; Code:
(define-module (vcomponent media-type common)
  :use-module (datetime)
  :use-module (vcomponent)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type period)
  :use-module (hnh util table)
  :use-module (ice-9 curried-definitions)
  :export (serialize-datetime
           serialize-period))



;;; NOTE this is identical to the matching in application/celandar+json
(define ((serialize-datetime dt-format) params v)
  (cond ((not (tz v))
         (datetime->string (tz v #f) dt-format))
        ((string=? "UTC" (tz v))
         (datetime->string v dt-format))
        (else
         (values (datetime->string (tz v #f) dt-format)
                 (table-put params 'TZID (tz v))))))

;; NOTE dt-format should contain a ~Z specifier, which is assumed to output "Z" for "UTC", and "" otherwise.
(define (serialize-period params v dt-format)
  ;; (tz start) MUST equal (tz end) (if end is a datetime object)
  (call-with-values (lambda () ((serialize-datetime dt-format) params (period-start v)))
    (lambda* (start optional: (params params))
      (values
       start
       (if (datetime? (period-end v))
           (datetime->string
            (period-end v)
            dt-format)
           (duration->string (period-end v)))
       params))))



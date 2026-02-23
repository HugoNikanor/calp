(define-module (test datetime-duration)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util lens)
  :use-module (datetime core)
  :use-module (datetime duration))

;;; For each of
;;; - duration-week
;;; - duration-date
;;; Test the following lenses
;;; - duration-year*
;;; - duration-month*
;;; - duration-day*
;;; - duration-hour*
;;; - duration-minute*
;;; - duration-second*


'((datetime duration))

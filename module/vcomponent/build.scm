;;; Commentary:
;; Module for quickly building new vcomponents from code.
;; @example
;; (vevent
;;  summary: "This is a test event"
;;  dtstart: #2020-01-01T13:37:00
;;  children: (list
;;             (valarm ...)))
;;; Code:

(define-module (vcomponent build)
  :use-module (util)
  :use-module (vcomponent base)
  :use-module (srfi srfi-26)
  :use-module ((srfi srfi-88) :select (keyword->string)))

(define-public (vevent . body)    (apply vcomponent 'VEVENT    body))
(define-public (vcalendar . body) (apply vcomponent 'VCALENDAR body))
(define-public (valarm . body)    (apply vcomponent 'VALARM    body))

(define-public (vcomponent tag . rest)
  (define v (make-vcomponent tag))

  (let loop ((rem rest))
    (unless (null? rem)
      (if (eq? children: (car rem))
          (for-each (cut add-child! v <>) (cadr rem))
          (let ((symb (-> (car rem)
                          keyword->string
                          string-upcase
                          string->symbol)))
            (set! (prop v symb) (cadr rem))))
      (loop (cddr rem))))

  ;; Return
  v)



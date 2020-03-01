;;; Commentary:

;; This file should define all global configurable variables which
;; doesn't belong anywhere else. The config module should then import
;; this module, and set all configs as needed. The config module
;; should also be able to set configs gotten from other parts.

;;; Code:

(define-module (parameters))

(define (ensure pred?)
  (lambda (v)
    (unless (pred? v)
      (error (format #f "Value [~s] doesn't satisfy condition ~a"
               v (or (procedure-name pred?) ""))))
    v))

(define-public calendar-files
  (make-parameter
   '() (ensure list?)))

(define-public summary-filter
  (make-parameter (lambda (_ a) a) (ensure procedure?)))

;; ev x str -> sxml
(define-public description-filter
  (make-parameter (lambda (_ a) a) (ensure procedure?)))

(use-modules (datetime util))

(define-public week-start
  (make-parameter sun (ensure (lambda (x) (<= sun x sat)))))

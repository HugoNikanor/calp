;;; Commentary:
;;; Data types shared between the different parts of the
;;; iCalendar parser.
;;; Code:
(define-module (vcomponent media-type text calendar parse-types)
  :use-module (hnh util object)
  :use-module ((hnh util type) :select (false?))
  :use-module (srfi srfi-88)
  :export (logical-line
           logical-line?
           logical-line-content logical-line-content*
           logical-line-file    logical-line-file*
           logical-line-line    logical-line-line*

           content-line
           content-line?
           content-line-metadata content-line-metadata*
           content-line-data     content-line-data*
           ))


(define-type (logical-line)
  (logical-line-content keyword: content type: string?)
  (logical-line-file keyword: file type: (or string? false?)
                     default: #f)
  (locical-line-line keyword: line type: (or exact-integer? false?)
                     default: #f))

(define-type (content-line)
  (content-line-metadata keyword: meta type: logical-line?)
  ;; (property-name (parameter-name . parameter-value) ... content)
  (content-line-data keyword: data type: list?))

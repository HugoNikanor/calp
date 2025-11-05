;;; Commentary:
;;; Wrapper unknown datatypes in iCalendar streams.
;;; Code:
(define-module (vcomponent type unknown)
  :use-module (hnh util object)
  :use-module (srfi srfi-88)
  :export (unknown
           unknown?
           from-unknown from-unknown*))

(define-type (unknown constructor: (lambda (o _) o)
                      serializer: (lambda (o) `(unknown ,(serialize (from-unknown o)))))
  (from-unknown type: string?))

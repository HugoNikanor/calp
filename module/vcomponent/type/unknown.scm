;;; Commentary:
;;; Wrapper unknown datatypes in iCalendar streams.
;;; Code:
(define-module (vcomponent type unknown)
  :use-module ((hnh util) :select (when))
  :use-module ((hnh util type) :select (false?))
  :use-module (hnh util object)
  :use-module (hnh util serialize)
  :use-module (srfi srfi-88)
  :export (unknown
           unknown?
           from-unknown from-unknown*
           unknown-type unknown-type*))

(define-type (unknown constructor: (lambda (constructor typecheck)
                                     (lambda* (value optional: type)
                                       (typecheck value type)
                                       (constructor value type)))
                      serializer: (lambda (o) `(unknown ,(serialize (from-unknown o))
                                                   ,@(when (unknown-type o)
                                                       (list
                                                        (serialize (unknown-type o)))))))
  (from-unknown type: string?)
  (unknown-type type: (or string? false?)))

;;; Should really be application/vnd.guile-read, but periods aren't
;;; allowed in guile module names.
(define-module (vcomponent media-type application vnd-guile-read)
  :use-module ((hnh util object) :select (serialize with-serializers))
  :use-module (vcomponent media-type)
  :use-module (ice-9 sandbox)
  :use-module (srfi srfi-88)
  :use-module ((web uri) :select (uri->string uri?))
  :export (format parse-modules))

(define-once parse-modules
  (make-parameter
   `(((hnh util) ->)
     ((hnh util table) table table-put)
     ((datetime) date time datetime)
     ((datetime timespec) timespec)
     ((vcomponent) vline vcomponent)
     ((vcomponent type duration) duration)
     ((vcomponent type geo) geo)
     ((vcomponent type period) period)
     ((vcomponent type recurrence) recur-rule)
     ((vcomponent type request-status) request-status)
     ((vcomponent type version) vcalendar-version)
     ((web uri) string->uri)) ))

(define format
  (calendar-data-format
   serializer:
   (lambda (r p)
     ;; NOTE this isn't configurable, since the general serialization API
     ;; already exists, and we are just using that.
     (with-serializers
      ((uri? (lambda (u) `(string->uri ,(uri->string u)))))
      (write (serialize r) p)))
   parser: (lambda (p)
             (eval-in-sandbox
              (read p)
              bindings: (append (parse-modules) all-pure-bindings)))))

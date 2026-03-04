;;; Should really be application/vnd.guile-read, but periods aren't
;;; allowed in guile module names.
(define-module (vcomponent media-type application vnd-guile-read)
  :use-module ((hnh util serialize)
               :select (serialize with-serializers))
  :use-module (vcomponent media-type)
  :use-module (ice-9 sandbox)
  :use-module (srfi srfi-88)
  :use-module ((web uri) :select (uri->string uri?))
  :export (format parse-modules))

(define-once parse-modules
  (make-parameter
   `(
     ((datetime)
      date time datetime tz
      mon tue wed thu fri sat sun
      )
     ((vcomponent create)
      create-vcomponent with-parameters
      vcalendar vevent vtodo vjournal vfreebusy
      vtimezone valarm standard daylight)
     ((vcomponent type duration) duration)
     ((vcomponent type geo) geo)
     ((vcomponent type period) period)
     ((vcomponent type recurrence) recur-rule)
     ((vcomponent type request-status) request-status)
     ((vcomponent type version) vcalendar-version)
     ((vcomponent type utc-offset) utc-offset)
     ((vcomponent type unknown) unknown)
     ((web uri) string->uri))))

(define format
  (calendar-data-format
   media-type: "application/vnd.guile-read"
   file-extension: "sexp"
   serializer:
   (lambda* (r p key: pretty?)
     (when pretty?
       (display ";;; -*- mode: scheme -*-\n" p))
     ;; NOTE this isn't configurable, since the general serialization API
     ;; already exists, and we are just using that.
     (with-serializers
      ((uri? (lambda (u) `(string->uri ,(uri->string u)))))
      ((if pretty?
           (@ (ice-9 pretty-print) pretty-print)
           write)
       (serialize r)
       p)))
   parser: (lambda (p)
             (eval-in-sandbox
              (read p)
              bindings: (append (parse-modules) all-pure-bindings)))))

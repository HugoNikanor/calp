(define-module (datetime instance)
  :use-module (calp util)
  :use-module (calp util config)
  :use-module (calp util exceptions)
  :use-module (datetime zic)
  :use-module ((xdg basedir) :prefix xdg-)
  :export (zoneinfo))

(define-config tz-list '()
  description: "List of default zoneinfo files to be parsed")

;; TODO see (vcomponent instance), this has a similar problem with early load
(define-once zoneinfo
  (let ((cache (make-hash-table)))
    (label self
           (case-lambda
             (()
              (define tz-list (get-config 'tz-list))
              (if (null? tz-list)
                (warning "Default zoneinfo only available when tz-dir and tz-list are configured")
                (self tz-list)))
             ((file-list)
              (provide 'zoneinfo)
              (let* ((directory (path-append (xdg-data-home) "/calp/zoneinfo"))
                     (key (cons directory file-list)))
                (aif (hash-ref cache key)
                     it
                     (let ((tz (read-zoneinfo
                                 (map (lambda (s) (path-append directory s))
                                      file-list))))
                       (hash-set! cache key tz)
                       tz))))))))

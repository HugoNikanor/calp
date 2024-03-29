(define-module (datetime instance)
  :use-module (hnh util)
  :use-module (calp util config)
  :use-module (hnh util exceptions)
  :use-module ((hnh util path) :select (path-append))
  :use-module (datetime zic)
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module (calp translation)
  :export (zoneinfo))

(define-config tz-list '()
  description: (G_ "List of default zoneinfo files to be parsed"))

;; TODO see (vcomponent uil instance), this has a similar problem with early load
;; Takes a list of zoneinfo files relative
;; $XDG-DATA-HOME/calp/zoneinfo, which will probably be
;; '("tzdata/europe" "tzdata/afrifa" ...)
;; and builds all these into one giant zoneinfo database object
;; Note that scripts/tzget should be run beforehand, to download the
;; data
(define-once zoneinfo
  (let ((cache (make-hash-table)))
    (label self
           (case-lambda
             (()
              (define tz-list (tz-list))
              (if (null? tz-list)
                (warning (G_ "Default zoneinfo only available when tz-dir and tz-list are configured"))
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

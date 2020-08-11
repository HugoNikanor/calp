(define-module (datetime instance)
  :use-module (util)
  :use-module (util config)
  :use-module (datetime zic)
  :export (zoneinfo))

(define-config tz-dir #f
  description: "Directory in which zoneinfo files can be found")

(define-config tz-list '()
  description: "List of default zoneinfo files to be parsed")


(define / file-name-separator-string)

;; TODO see (vcomponent instance), this has a similar problem with early load
(define-once zoneinfo
  (let ((cache (make-hash-table)))
    (label self
     (case-lambda
       (()
        (define tz-dir (get-config 'tz-dir))
        (define tz-list (get-config 'tz-list))
        (when (or (not tz-dir) (null? tz-list))
          (error "Default zoneinfo only available when tz-dir and tz-list are configured"))
        (self tz-dir tz-list))
       ((directory file-list)
        (let ((key (cons directory file-list)))
          (aif (hash-ref cache key)
               it
               (let ((tz (read-zoneinfo
                          (map (lambda (s) (string-append directory / s))
                               file-list))))
                 (hash-set! cache key tz)
                 tz))))))))

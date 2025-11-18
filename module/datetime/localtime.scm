(define-module (datetime localtime)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (srfi srfi-1)
  :export (get-localtime))

(define (get-localtime)
  (or (getenv "TZ")
      ;; TODO capture errors if localtime is missing, or on other errors
      (-> (take-while
           (lambda (e) (not (string=? e "zoneinfo")))
           (reverse (path-split (readlink "/etc/localtime"))))
          reverse
          (string-join "/"))
      "UTC"))

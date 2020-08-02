(define-module (datetime instance)
  :use-module (util)
  :use-module (ice-9 rdelim)
  :use-module (datetime zic)
  :export (zoneinfo))


(define-once
  zoneinfo
  (let* ((pipe
          (-> (@ (global) basedir)
              dirname
              (string-append "/tzget")
              ((@ (ice-9 popen) open-input-pipe))))
         (path (read-line pipe))
         (names (string-split (read-line pipe) #\space)))
    (read-zoneinfo
     (map (lambda (s) (string-append path file-name-separator-string s))
          names))))

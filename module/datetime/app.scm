(define-module (datetime app)
  :use-module (util)
  :use-module (util app)
  :use-module (ice-9 rdelim)
  :use-module (datetime zic))

(define-method (init-app)
  (setf 'zoneinfo
        (let* ((pipe
                (-> (@ (global) basedir)
                    dirname
                    (string-append "/tzget")
                    ((@ (ice-9 popen) open-input-pipe))))
               (path (read-line pipe))
               (names (string-split (read-line pipe) #\space)))
          (read-zoneinfo
           (map (lambda (s) (string-append path file-name-separator-string s))
                names)))))

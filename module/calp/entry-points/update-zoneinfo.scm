(define-module (calp entry-points update-zoneinfo)
  :export (main)
  ;; :use-module (hnh util)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (hnh util path)
  :use-module ((hnh util io) :select (with-atomic-output-to-file))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (hnh util options)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 popen)
  :use-module (ice-9 format)
  :use-module (calp translation))

(define opt-spec
  `((help (single-char #\h) (description ,(G_ "Print this help.")))))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (let* ((locations (list "/usr/libexec/calp/tzget"
                                 (path-append (xdg-data-home) "tzget")))
         (filename (or (find file-exists? locations)
                              (scm-error 'missing-helper "update-zoneinfo"
                                         (G_ "tzget not installed, please put it in one of ~a")
                                         (list locations)
                                         (list "tzget" locations))))

         (pipe (open-input-pipe filename))
         (names (string-split (read-line pipe) #\space)))
    (with-atomic-output-to-file (path-append (xdg-data-home) "calp" "zoneinfo.scm")
      (lambda ()
        (format #t ";;; Autogenerated file~%;;; Last updated ~a~%~y~%"
                (datetime->string (current-datetime))
                `((@ (datetime instance) tz-list) (quote ,names)))))

    (close-pipe pipe)))

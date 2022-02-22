(define-module (vcomponent util parse-cal-path)
  :use-module (hnh util)
  :use-module ((calp util time) :select (report-time!))
  :use-module (vcomponent base)
  :use-module (calp translation)
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((vcomponent formats vdir parse)
               :select (parse-vdir)))


;; Parse a vdir or ics file at the given path.
(define-public (parse-cal-path path)
  ;; TODO check (access? path R_OK) ?
  (define st (stat path))
  (define cal
    (case (stat:type st)
      [(regular)
       (let ((comp (call-with-input-file path parse-calendar)))
         (set! (prop comp '-X-HNH-SOURCETYPE) 'file)
         comp) ]
      [(directory)
       (report-time! (_ "Parsing ~a") path)
       (let ((comp (parse-vdir path)))
         (set! (prop comp '-X-HNH-SOURCETYPE) 'vdir
               (prop comp '-X-HNH-DIRECTORY) path)
         comp)]
      [(block-special char-special fifo socket unknown symlink)
       => (lambda (t) (error (_ "Can't parse file of type ") t))]))

  (unless (prop cal "NAME")
    (set! (prop cal "NAME")
      (or (prop cal "X-WR-CALNAME")
          (string-append "[" (basename path) "]"))))

  cal)

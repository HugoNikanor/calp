(define-module (calp benchmark parse)
  :use-module (util)
  :use-module (glob)
  :use-module (statprof)

  :use-module ((srfi srfi-1) :select (concatenate))
  :use-module ((ice-9 ftw) :select (scandir))

  )

(define-public (run-benchmark)
  (define all-calendar-files
    (statprof
     (lambda ()
       (concatenate
        (map (lambda (path)
               (map
                (lambda (fname) (path-append path fname))
                (scandir path (lambda (s) (and (not (string= "." (string-take s 1)))
                                          (string= "ics" (string-take-right s 3)))))))
             (glob "~/.local/var/cal/*"))))))

  (define all-read
    (statprof
     (lambda ()
       (map (lambda ( fullname)
              (let ((cal (call-with-input-file fullname
                           (@@ (vcomponent ical parse) read-file))))
                cal))
            all-calendar-files))))

  (define tokenized
    (statprof
     (lambda ()
       (map (lambda (one-read)
              (map (@@ (vcomponent ical parse) tokenize)
                   one-read))
            all-read))))

  (define parsed
    (statprof
     (lambda ()
       (map (@@ (vcomponent ical parse) parse) tokenized))))

  (format #t "~a files processed~%"
          (length parsed))
  )

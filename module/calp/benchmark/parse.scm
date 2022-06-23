(define-module (calp benchmark parse)
  :use-module (hnh util)
  :use-module ((hnh util path) :select (path-append))
  :use-module (glob)
  :use-module (statprof)
  :use-module (datetime)

  :use-module ((srfi srfi-1) :select (concatenate))
  :use-module ((ice-9 ftw) :select (scandir))

  :export (run-benchmark)
  )

(define (run-benchmark)
  (define all-calendar-files
    (statprof
     (lambda ()
       (display "All calendar files\n")
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
       (display "All read\n")
       (map (lambda ( fullname)
              (let ((cal (call-with-input-file fullname
                           (@@ (vcomponent formats ical parse) read-file))))
                cal))
            all-calendar-files))))

  (define tokenized
    (statprof
     (lambda ()
       (display "Tokenized\n")
       (map (lambda (one-read)
              (map (@@ (vcomponent formats ical parse) tokenize)
                   one-read))
            all-read))))

  (define parsed
    (statprof
     (lambda ()
       (display "Parsed\n")
       (map (@@ (vcomponent formats ical parse) parse) tokenized))))

  (format #t "~a files processed~%"
          (length parsed))
  )

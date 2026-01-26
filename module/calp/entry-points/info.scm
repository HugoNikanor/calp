(define-module (calp entry-points info)
  :use-module (calp translation)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (hnh util path)
  :use-module (ice-9 getopt-long)
  :use-module (vcomponent media-type)
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module (vcomponent type duration)
  :export (%summary main))

(define %summary
  (G_ "Print info about given calendar file."))

(define-public %category 'static)

(define opt-spec
  `((format
     (single-char #\f)
     (value #t)
     (description
      ,(G_ "Explicit file format for given file.")))
    (help (single-char #\h)
          (description
           ,(G_ "Print this help.")))))

(define (vcomponent-attempt-id vcomponent)
  (case (type vcomponent)
    ((VCALENDAR) (or (prop1 vcomponent 'PRODID) "(PRODID missing)"))
    ((VEVENT) (string-append
               (or (prop1 vcomponent 'UID) "-")
               (let ((rid (prop1 vcomponent 'RECURRENCE-ID)))
                (cond ((datetime? rid)
                       ;; TODO TZ
                       (string-append ", " (datetime->string rid)))
                      ((date? rid)
                       (string-append ", " (date->string rid)))
                      (else "")))))
    ((VALARM)
     (format #f "~a ~a"
             (prop1 vcomponent 'ACTION)
             (let ((tr (prop1 vcomponent 'TRIGGER)))
               (cond ((datetime? tr) (datetime->string tr "~1 ~3"))
                     ((duration? tr) (duration->string tr))
                     (else tr)))))
    ((VJOURNAL) (or (prop1 vcomponent 'UID) "-"))
    ((FREEBUSY) (or (prop1 vcomponent 'UID) "-"))
    ((VTIMEZONE) (or (prop1 vcomponent 'TZID) "-"))
    ((STANDARD DAYLIGHT)
     (format #f "~a, from ~a to ~a"
             (datetime->string (prop1 vcomponent 'DTSTART) "~1 ~3")
             (timespec->string (prop1 vcomponent 'TZOFFSETFROM) 'm)
             (timespec->string (prop1 vcomponent 'TZOFFSETTO)   'm)))
    (else "")))

(define* (print-vcomponent-tree vcomponent optional: (depth 0))
  (format #t "~a- ~a ~a~%"
          (make-string (* 2 depth) #\space)
          (type vcomponent)
          (vcomponent-attempt-id vcomponent))
  (for-each (lambda (child)
              (print-vcomponent-tree child (1+ depth)))
            (vcomponent-children vcomponent)))

(define (get-media-format override filename)
  (module-ref
   (resolve-interface
    `(vcomponent
      media-type
      ,@(if override
            (map string->symbol (string-split override #\/))
            (case (-> filename filename-extension
                      string-downcase string->symbol)
              ((ics ifb) '(text calendar))
              ((json)    '(application calendar+json))
              ((xcs xml) '(application calendar+xml))
              ((sexp)    '(application vnd-guile-read))
              (else
               => (lambda (it)
                    (scm-error 'misc-error "get-media-format"
                               "Unknown media type: ~s"
                               (list it) (list filename))))))))
   'format))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (when (option-ref opts 'help #f)
    (print-arg-help opt-spec)
    (throw 'return))

  (for-each (lambda (filename)
              ;; figure out file type
              (define media-format
                (get-media-format (option-ref opts 'format #f)
                                  filename))
              ;; read file
              (define top-level
                (call-with-input-file filename (parser media-format)))

              ;; display file info
              (format #t "~a~%" filename)
              (print-vcomponent-tree top-level))
            (option-ref opts '() '())))

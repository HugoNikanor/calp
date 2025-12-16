(define-module (test media-type run)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((hnh util) :select (-> for print-and-return))
  :use-module (hnh util path)
  :use-module ((ice-9 rdelim) :select (read-string))
  :use-module (ice-9 pretty-print)
  :use-module (rnrs io ports)
  :use-module (datetime)
  :use-module (vcomponent create)
  :use-module (vcomponent media-type)
  :use-module ((vcomponent media-type text calendar) :prefix #{ics:}#)
  :use-module ((vcomponent media-type application calendar+xml) :prefix #{xcs:}#)
  :use-module ((vcomponent media-type application calendar+json) :prefix #{jcal:}#)
  :use-module ((vcomponent) :select (vcomponent-diff))
  :use-module ((vcomponent type version))
  :use-module (sxml namespaced)
  :use-module ((calp namespaces) :select (xcal))
  :use-module (hnh test xmllint)

  ;; Requirements for the reference component
  :use-module ((hnh util) :select (->))
  :use-module (datetime)
  :use-module (vcomponent create)
  :use-module (rnrs io ports)

  :use-module (vcomponent)
  :use-module (hnh util table)
  :use-module (datetime timespec)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type period)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type unknown)
  )

;;; TODO multi-valued fields
;;; TODO validate event for correctness here:
;;; - All fields have expected values

;;; TODO Double quote symbols (ASCII 0x22) CAN NEVER appear in
;;; parameters, since they lack any escape mechanism. This technically
;;; only applies to iCalendar itself, since all other formats are assumed
;;; to have real escape rules.



(define* (run-test test-name reference-object serialized-file media-type* key: (formatter identity))

  ;; Assert serialize is set

  ;; String representation of pre-vetted representation of the format
  ;; (e.g. target.ics)
  (define target
    (call-with-input-file (path-append (dirname (current-filename))
                                       serialized-file)
      read-string))

  ;; The reference component (defined above),
  ;; serialized into the target format
  (define serialized-component
    (call-with-output-string
      (lambda (port) ((serializer media-type*) reference-object port))))

  ;; Check that the serialization suceeded
  (test-equal (format #f "Serialize ~s" test-name)
    (formatter target)
    (formatter serialized-component))

  ;; If a parser is given, check that re-parsing the serialized component
  ;; returns the original component.
  (cond ((parser media-type*)
         => (lambda (parse)
              (test-equal (format #f "Parse ~s" test-name)
                (cond
                 ((and (equal? "text/calendar" (media-type media-type*))
                       (string=? "hand-written/types" test-name))
                  ;; iCalendar properties with unknown types are
                  ;; treated defaulting to strings, which means that a
                  ;; unknown field explicitly tagged as being of type
                  ;; TEXT is indistinguishable from one without a type
                  ;; tag. All other formats explicitly differentiate
                  ;; between these with mandatory type tags.
                  ;;
                  ;; Changing the .sexp file to contain an `unknown`
                  ;; value would give the same problem in the other
                  ;; direction, and error on all other types.
                  `((diff X-TEXT
                          (,(vline value: "This is some text"))
                          (,(vline value: (unknown "This is some text"))))))
                 (else '()))
                (vcomponent-diff
                 reference-object
                 (call-with-input-string target parse)
                 ))))))




(for file in (list
              "hand-written/target"
              "hand-written/types"
              "hand-written/quoted-params"
              "hand-written/unknown-value-type"
              "hand-written/x-integer"
              "rfc-provided/ex1"
              "rfc-provided/ex2"
              ;; "hand-written/monetary"
              )

     (test-group file

       (define reference
         (call-with-input-file (path-append (dirname (current-filename))
                                            (string-append file ".sexp"))
           (parser (@ (vcomponent media-type application vnd-guile-read) format))))

       (test-group "iCalendar"
         ;; Set linewrap to "infinity", to allow easier diffs
         (parameterize (((@ (vcomponent media-type text calendar output) icalendar-wrap-length) 1000))
           (run-test
            file
            reference
            (string-append file ".ics")
            ics:format)))

       (test-group "xCal"
         (run-test
          file
          reference
          (string-append file ".xcs")
          xcs:format
          formatter: xmllint))

       (unless (provided? 'formats-jcal)
         (test-skip "jCal"))

       (test-group "jCal"
         (run-test
          file
          reference
          (string-append file ".json")
          jcal:format
          formatter:
          (lambda (v)
            (with-output-to-string
              (lambda ()
                (-> (call-with-input-string v (@ (json) json->scm))
                    ((@ (json) scm->json) pretty: #t)))))))

       ))

'((vcomponent media-type application calendar+xml)
  (vcomponent media-type application calendar+xml output)
  (vcomponent media-type application calendar+xml parse)

  (vcomponent media-type application calendar+json)
  (vcomponent media-type application calendar+json output)
  (vcomponent media-type application calendar+json parse)

  (vcomponent media-type application vnd-guile-read)

  (vcomponent media-type text calendar)
  (vcomponent media-type text calendar output)
  (vcomponent media-type text calendar parse)
  )

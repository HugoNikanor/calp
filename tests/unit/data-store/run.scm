(define-module (tests data-store run)
  :use-module (srfi srfi-64)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (web uri)
  :use-module (datetime)
  :use-module ((vcomponent) :select (vcomponent-diff))
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type))

;;; Tests of all data stores

;;; - create a blank data store
;;; - store entry
;;; - retrieve stored entry
;;; - check for equality

;;; - overwrite existing entry

;;; - delete entry
(define generate-href
  (let ((counter 0))
    (lambda ()
      (set! counter (1+ counter))
      (format #f "href-~a.ics" counter))))

(define entries
  (for file in (list
                "hand-written/target.ics"
                "hand-written/types.ics"
                "rfc-provided/ex1.ics"
                "rfc-provided/ex2.ics"
                ;; "hand-written/monetary.ics"
                )

       (cons (generate-href)
             (call-with-input-file
                 (path-append
                  (dirname (dirname (current-filename)))
                  "formats" file)
               (parser
                (@ (vcomponent media-type text calendar) format))))))



(define testdir (mkdtemp "/tmp/calp-store-XXXXXX"))

(for uri in (list
             (format #f "store:file?path=~a&media=text/calendar"
                     (path-append testdir "path-store.ics"))
             (format #f "store:vdir?path=~a&media=text/calendar"
                     (path-append testdir "vdir-store"))
             (format #f "store:sqlite?path=~a"
                     (path-append testdir "sqlite-store.db"))
             )

     (test-group uri
       (let ((store (-> uri string->uri store-uri->store)))
         (for-each (lambda (entry)
                     (put-event! store (car entry) (cdr entry)))
                   entries)
         (flush! store))
       ;; We close and re-open the store, to ensure we read from storage
       ;; instead of internal caches.
       (let ((store (-> uri string->uri store-uri->store)))
         (for (href . reference) in entries
              (test-equal href
                '()
                (vcomponent-diff
                 reference
                 (get-by-href store href))))
         ))

     )


'()

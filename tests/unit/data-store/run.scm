(define-module (tests data-store run)
  :use-module (srfi srfi-64)
  :use-module (ice-9 regex)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (hnh util destructure)
  :use-module (hnh util optional)
  :use-module (web uri)
  :use-module (datetime)
  :use-module ((vcomponent) :select (vcomponent-diff vline))
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type)
  :use-module (vcomponent type version)
  :use-module ((datetime zoneinfo)
               :select (read-zoneinfo intermediary->zoneinfo))
  )

;;; Tests of all data stores

;;; - create a blank data store
;;; - store entry
;;; - retrieve stored entry
;;; - check for equality

;;; - TODO overwrite existing entry
;;; - TODO delete entry

;;; - TODO check how they manage an external part modifying them
;;;   + new entries added, but without hrefs set
;;;   + entries deleted
;;;   + entries changed
;;;   + invalid data added


;;; TODO TODO
;;; This currently goes on the built-in timezone database.
;;; This means that many of the tests may fail due to a bad
;;; database being installed.

;;; For the time being, we should install a database with the needed zones,
;;; but in the future instead install an empty one ensuring that all
;;; timezone data is read from the calendar files.

(zoneinfo
 (call-with-input-string "

# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone America/New_York	-4:56:02 -	LMT	1883 Nov 18 17:00u
			-5:00	US	E%sT	1920
			-5:00	NYC	E%sT	1942
			-5:00	US	E%sT	1946
			-5:00	NYC	E%sT	1967
			-5:00	US	E%sT

Link America/New_York US/Eastern

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER
Rule	NYC	1920	only	-	Mar	lastSun	2:00	1:00	D
Rule	NYC	1920	only	-	Oct	lastSun	2:00	0	S
Rule	NYC	1921	1966	-	Apr	lastSun	2:00	1:00	D
Rule	NYC	1921	1954	-	Sep	lastSun	2:00	0	S
Rule	NYC	1955	1966	-	Oct	lastSun	2:00	0	S

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER/S
Rule	US	1918	1919	-	Mar	lastSun	2:00	1:00	D
Rule	US	1918	1919	-	Oct	lastSun	2:00	0	S
Rule	US	1942	only	-	Feb	9	2:00	1:00	W # War
Rule	US	1945	only	-	Aug	14	23:00u	1:00	P # Peace
Rule	US	1945	only	-	Sep	30	2:00	0	S
Rule	US	1967	2006	-	Oct	lastSun	2:00	0	S
Rule	US	1967	1973	-	Apr	lastSun	2:00	1:00	D
Rule	US	1974	only	-	Jan	6	2:00	1:00	D
Rule	US	1975	only	-	Feb	lastSun	2:00	1:00	D
Rule	US	1976	1986	-	Apr	lastSun	2:00	1:00	D
Rule	US	1987	2006	-	Apr	Sun>=1	2:00	1:00	D
Rule	US	2007	max	-	Mar	Sun>=8	2:00	1:00	D
Rule	US	2007	max	-	Nov	Sun>=1	2:00	0	S
" (compose intermediary->zoneinfo read-zoneinfo)))



(define entries
  (for file in (list
                "hand-written/target.ics"
                "hand-written/types.ics"
                "hand-written/unknown-value-type.ics"
                "hand-written/x-integer.ics"
                "rfc-provided/ex1.ics"
                "rfc-provided/ex2.ics"
                ;; "hand-written/monetary.ics"
                )

       (list (regexp-substitute/global
              #f "/" file
              'pre "-" 'post)
             file
             (call-with-input-file
                 (path-append
                  (dirname (dirname (current-filename)))
                  "media-type" file)
               (parser
                (@ (vcomponent media-type text calendar) format))))))



(define testdir (mkdtemp "/tmp/calp-store-XXXXXX"))

(define uris
  (list
   (build-uri 'store host: "file" path: (path-append testdir "path-store.ics")
              query: "media=text/calendar")
   (build-uri 'store host: "vdir" path: (path-append testdir "vdir-store")
              query: "media=text/calendar")))

;;; Explicitly load the module, since the feature test won't work otherwise
(use-modules (vcomponent data-stores sqlite))
(when (provided? 'data-store-sqlite)
  (set! uris
    (append uris
            (list
             (build-uri 'store host: "sqlite"
                        path: (path-append testdir "sqlite-store.db"))))))

(for uri in uris

     (test-group (uri->string uri)
       (let ((store (store-uri->store uri)))
         (for-each (lambda (entry)
                     (put-event! store (list-ref entry 0) (list-ref entry 2)))
                   entries)
         (flush! store)
         (close-store! store))

       ;; We close and re-open the store, to ensure we read from storage
       ;; instead of internal caches.
       (let ((store (store-uri->store uri)))
         (for (href source-filename reference-entry) in entries
              (test-equal href
                ;; The file data store can't store properties in the VCALENDAR envolope,
                ;; And instead generates its own minimal one on output. This causes all
                ;; these diffs.
                (cond ((and (string=? "file" (uri-host (store-uri store)))
                            (string=? source-filename "rfc-provided/ex1.ics"))
                       `((*properties*
                          PRODID
                          (,(vline value: "-//Example Inc.//Example Calendar//EN"))
                          (,(vline value: "-//hugo//calp 0.6.1//EN")))))

                      ((and (string=? "file" (uri-host (store-uri store)))
                            (string=? source-filename "rfc-provided/ex2.ics"))
                       `((*properties*
                          PRODID
                          (,(vline value: "-//Example Corp.//Example Client//EN"))
                          (,(vline value: "-//hugo//calp 0.6.1//EN")))
                         (*properties* CALSCALE _ b)))

                      ((and (string=? "file" (uri-host (store-uri store)))
                            (string=? source-filename "hand-written/target.ics"))
                       `((*properties*
                          PRODID
                          (,(vline value: "-//CALP-TEST//x.y"))
                          (,(vline value: "-//hugo//calp 0.6.1//EN")))
                         (*properties* REQUEST-STATUS a _)))

                      ((and (string=? "file" (uri-host (store-uri store)))
                            (string=? source-filename "hand-written/types.ics"))
                       `((*properties* GEO a _)
                         (*properties* REQUEST-STATUS a _)
                         (*properties*
                          VERSION
                          (,(vline value: (vcalendar-version min: "2.0" max: "3.0")))
                          (,(vline value: (vcalendar-version max: "2.0"))))
                         (*properties* X-BINARY a _)
                         (*properties* X-BOOLEAN a _)
                         (*properties* X-CAL-ADDRESS a _)
                         (*properties* X-DATE a _)
                         (*properties* X-DATE-TIME a _)
                         (*properties* X-DURATION a _)
                         (*properties* X-FLOAT a _)
                         (*properties* X-INTEGER a _)
                         (*properties* X-PERIOD a _)
                         (*properties* X-RECUR a _)
                         (*properties* X-TEXT a _)
                         (*properties* X-TIME a _)
                         (*properties* X-UNKNOWN a _)
                         (*properties* X-URI a _)
                         (*properties* X-UTC-OFFSET a _)
                         (*properties* CALSCALE _ b)
                         (*properties* PRODID _ b)))

                      ((and (string=? "file" (uri-host (store-uri store)))
                            (string=? source-filename "hand-written/unknown-value-type.ics"))
                       `((*properties* CALSCALE _ b)
                         (*properties* PRODID _ b)))


                      ((and (string=? "file" (uri-host (store-uri store)))
                            (string=? source-filename "hand-written/x-integer.ics"))
                       `((*properties* CALSCALE _ b)
                         (*properties* PRODID _ b)))

                      (else '()))
                (vcomponent-diff
                 reference-entry
                 (get-by-href store href)
                 table-report: (destructure-lambda*
                                ((list key (just a) (just b)) `(,key ,a ,b))
                                ((list key (nothing) _) `(,key _ b))
                                ((list key _ (nothing)) `(,key a _))))))
         )))


'()

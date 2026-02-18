(define-module (test rfc4791 run)
  :use-module (web http)
  :use-module (web uri)
  :use-module (web request)
  :use-module (web response)

  :use-module (glob)

  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)

  :use-module (datetime timezone)
  :use-module (datetime timespec)
  :use-module ((datetime zoneinfo)
               :select (read-zoneinfo intermediary->zoneinfo))

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-197)

  :use-module ((rnrs base) :select (assert) :version (6))

  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (hnh util type)
  :use-module (hnh util table)
  :use-module (hnh util optional)

  :use-module (ice-9 rdelim)
  :use-module (ice-9 curried-definitions)

  :use-module ((calp server webdav) :select (webdav-handler))
  :use-module (calp webdav builder)
  :use-module ((calp namespaces) :select (webdav caldav))

  :use-module ((vcomponent) :select (vcomponent-diff))
  :use-module ((vcomponent media-type) :select (resolve-media-type parser))

  )

;;; All request-response examples from the RFC have been extracted and
;;; placed into files. For each of these pairs, we check if it's a
;;; REPORT request. If so, then we "run" it, by manually invoking the
;;; HTTP handler.
;;; Running the handler manually removes any issues with initializing
;;; a server, and allows much easier backtraces when things crash


;;; We install a dummy zoneinfo database containing what the samples expect.
;;; TODO this MUST be changed to the using zoneinfo from the store/line correctly.
;;; Our vendored zoneinfo database should ONLY be used when the user wants to
;;; change to a new zone:w

(define ((tag-matches*? namespace tagname) tag)
  (tag-matches? tag tagname namespace))


;;; Calculates the diff between tho <DAV:response /> objects.
;;; The objects are assumed to already point to the same href
;;; Only specific fields are compared
(define (dav-response-diff a b)
  ;; xpath
  ;; //D:response/D:propstat[D:status/text()="HTTP/1.1 200 OK"]/D:prop/C:calendar-data/text()
  (define (get-calendar-data response)
    (assert (tag-matches? response 'response webdav))
    (chain-and response
               (xml-element-children _)
               (find (lambda (el)
                       (and (tag-matches? el 'propstat webdav)
                            (and=>
                             (find (tag-matches*? webdav 'status)
                                   (xml-element-children el))
                             (lambda (el) (string=? "HTTP/1.1 200 OK" (xml-text-content el))))))
                     _)
               (xml-element-children _)
               (find (tag-matches*? webdav 'prop) _)
               (xml-element-children _)
               (find (tag-matches*? caldav 'calendar-data) _)
               (xml-text-content _)))

  (define media-format (resolve-media-type "text/calendar"))

  (let ((a-data (get-calendar-data a))
        (b-data (get-calendar-data b)))
    ;; NOTE this is a hack. In test 7.9.1, a 404 clause is
    ;; present. get-calendar-data (obviously) fails to find the
    ;; calendar data, meaning that both are equal then. A more thurrow
    ;; check should be implemented.
    (if (equal? a-data b-data)
        '()
        (vcomponent-diff
         (call-with-input-string a-data (parser media-format))
         (call-with-input-string b-data (parser media-format))))))


(define (multistatus-diff a b)
  (typecheck a xml-document?)
  (typecheck b xml-document?)

  (assert (tag-matches? (xml-document-root a) 'multistatus webdav))
  (assert (tag-matches? (xml-document-root b) 'multistatus webdav))

  (define (key-proc el)
    (and=>
     (find (lambda (el) (tag-matches? el 'href webdav))
           (xml-element-children el))
     (compose string->symbol
              ;; the examples have full URI hrefs
              ;; (http://cal.example.com/bernard/work/abcd3.ics),
              ;; while we only return the local component (abcd3.ics).
              ;; This is a hack to get them to match
              (lambda (s) (last (string-split s #\/)))
              xml-text-content)))

  (let ((left-only
         both right-only
         (let ((f (lambda (x) (cons (key-proc x) x))))
           (table-venn-partition
            (alist->table (map f (xml-element-children (xml-document-root a))))
            (alist->table (map f (xml-element-children (xml-document-root b))))))))

    (append (table->list left-only  (lambda (_ x) (cons 'left-only  x)))
            (table->list right-only (lambda (_ x) (cons 'right-only x)))
            (filter
             (lambda (p) (not (null? (cdr p))))
             (table->list
              both (lambda (_ x)
                     (cons* (and=> (find (tag-matches*? webdav 'href)
                                         (xml-element-children (car x)))
                                   xml-text-content)
                            (dav-response-diff (car x) (cdr x)))))))))



(define handler
  (webdav-handler
   (build-webdav-resource-tree
    ;; Resource tree created to mimic what the RFC samples expect.
    ;; /bernard is currently a <calendar-home>, but could in theory be anything
    `(virtual
      (("bernard"
        (calendar-home
         (("work"
           (calendar-collection
            ,(format #f "store://webdav-report-xml~a/webdav-report.xml"
                     (dirname (current-filename)))))))))))))

;;; TODO run these tests for ALL data stores

;; "5.3.1.2"                             ; MKCALENDAR
;; "5.3.2"                               ; PUT

;; Tests error on unsupported filter. However, we support all filters
;; "7.8.10"

(define test-dirs
 '(
   "appendix-b"
   "7.8.1"
   "7.8.2"

   ;; TODO Fails since expand isn't properly implemented
   "7.8.3"

   "7.8.4"                              ; VFREEBUSY
   "7.8.5"                              ; VTODO & VALARM
   "7.8.6"
   "7.8.7"
   "7.8.8"
   "7.8.9"

   "7.9.1" ; REPORT calendar-multiget

   ;; TODO free-busy-report not implemented
   ;; "7.10.1" ; REPORT free-busy-report
   ))

(define fake-zoneinfo (call-with-input-string "

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



(test-group "RFC 4791 (CalDAV) tests"
  (parameterize ((zoneinfo fake-zoneinfo)
                 ;; TODO we should maybe capture the logs and display
                 ;; them in case of errors.
                 ((@ (calp server webdav) current-log-port)
                  (%make-void-port OPEN_WRITE)))
    (for test-dir in test-dirs
         (define path (path-append (dirname (current-filename)) test-dir))
         (test-group (basename path)

           (define request-file  (open-input-file (path-append path "request")))
           (define response-file (open-input-file (path-append path "response")))

           (define request (read-request request-file))
           (define expected-response (read-response response-file))
           (define expected-response-body
             (read-delimited "" (response-port expected-response)))

           (call-with-values
               (lambda () (handler request (read-delimited "" (request-port request))))

             (lambda* (headers optional: (body "") state)
               (test-group "Headers"
                 (test-equal "code"
                   (response-code expected-response)
                   (response-code headers))

                 (test-equal "expected content-type"
                   'application/xml
                   (and=> (assoc-ref (response-headers expected-response) 'content-type) car))

                 (test-equal "actual content-type"
                   'application/xml
                   (and=> (assoc-ref (response-headers headers) 'content-type) car)))

               (if (eq? 'application/xml
                        (and=> (assoc-ref (response-headers expected-response) 'content-type) car)
                        (and=> (assoc-ref (response-headers headers) 'content-type) car))
                   (let ((a* (xml->namespaced-sxml expected-response-body))
                         (b* (xml->namespaced-sxml body)))
                     ;; (format #t "=== a ===~%~a~%" a*)
                     ;; (format #t "=== b ===~%~a~%" b*)
                     (test-equal "Equivalent content"
                       '() (multistatus-diff a* b*)))
                   (test-equal "Equivalent (when error)"
                     expected-response-body body))))))))

'(
  (vcomponent data-stores report-canonical)
  ;; TODO do we cover anything more?
  )

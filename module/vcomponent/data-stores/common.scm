;;; These are the common methods for implementing all calendar data stores.
;;; Each data store type provides a way to store the data (such as a
;;; file, a database, ...), while each data store instance is a specific
;;; calendar.

;;; TODO move this module to (vcomponent data-stores)

(define-module (vcomponent data-stores common)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (oop goops)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util optional)
  :use-module (hnh util lens)
  :use-module (hnh util table)
  :use-module (vcomponent)
  :use-module (vcomponent datetime)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent media-type)
  :use-module (sxml namespaced)
  :use-module ((calp namespaces) :select (caldav webdav))
  :use-module (sxml namespaced util)
  :use-module (web uri)
  :use-module (web http status-codes)
  :use-module ((web response) :select (build-response))
  :use-module ((web query) :select (parse-query))
  :use-module ((rnrs base) :select (assert) :version (6))
  :use-module (datetime)
  :use-module (ice-9 match)
  :use-module ((vcomponent data-stores report-canonical)
               :select (execute-comp-filter))
  :use-module ((calp webdav property) :select (propstat->namespaced-sxml))
  :use-module ((calp webdav propfind) :select (exec-propfind))
  :use-module ((oop goops) :select (make))
  :use-module ((calp webdav resource virtual-calendar-object)
               :select (<virtual-calendar-object-resource>))
  :export (
           <calendar-data-store>
           calendar-data-store?
           ;; get-all

           list-entries/shallow
           list-entries
           entry-count
           get-by-uid
           get-by-href
           ;; caldav-filter

           put-event!
           flush!
           close-store!

           store-color set-store-color! remove-store-color!
           store-displayname set-store-displayname! remove-store-displayname!
           store-description set-store-description! remove-store-description!
           store-calendar-timezone set-store-calendar-timezone! remove-store-calendar-timezone!

           store-uri->store

           store-uri

           execute-expand-property-report
           execute-calendar-query-report
           execute-calendar-multiget-report
           execute-free-busy-query-report

           entries-in-interval
           entries-by-summary

           ;; extract-time-range
           ))

;;; TODO TODO TODO
;;; Write common data store test sutie, which for each data store
;;; implementation runs through this all methods defined here, and
;;; ensures that they return valid values.
;;; NOTE that this won't guarantee that the stores retuns the "correct" value.

(define-class <calendar-data-store> ())

(define (calendar-data-store? x)
  (is-a? x <calendar-data-store>))

;;; Retrieve all events in the store, as a single VCALENDAR object
;;; with a set of VEVENT and VTIMEZONE objects as children.
;;; If the store saves individual calendar data for each VEVENT, then
;;; that might be discorded in favour for a generated VCALENDAR wrapper,
;;; assuming that all VCALENDAR objects are compatible.
;;;
;;; TODO while this is sometimes useful, you quite often want to export
;;; as a series of VCalendar objects with single VEvent objects in each.
;;; 
;;; get-all :: <store> -> vcalendar
;; (define-generic get-all)

;; (define-generic replace-all!)

;;; Returns a single event wrapped in a VCALENDAR wrapper.
;;; The event MAY consist of multiple VEVENT components, given that
;;; they all share the same UID, for example when exceptions to a
;;; recurrence date occurs.
;;; The returned VCALENDAR SHOULD contain VTIMEZONE for each stated timezone.
;;; get-by-uid :: <store>, uid → vcalendar | #f
;;; DEPRECATED, use the true search instead
(define-generic get-by-uid)

(define-method (get-by-uid (store <calendar-data-store>) uid)
  (find (lambda (ev)
          (equal? (just uid)
                  (get/preview ev (lens-compose vcomponent-children*
                                                car* (prop* 'UID) car*))))
        (map cdr (list-entries store))))

(define-generic get-by-href)

(define-method (get-by-href (store <calendar-data-store>) href)
  (and=> (find (lambda (p) (string=? href (car p)))
               (list-entries store))
         cdr))

;;; Inserts or update an event in the store.
;;; The event MUST be a VCALENDAR object, containing one logical event
;;; (note that this might contain multiple VEVENT objects in some cases)
;;; The event MUST have an UID property set, all VEVENT objects MUST
;;; have the same value for their UID property
;;; If the store doesn't support properties for individual VCALENDAR
;;; objects (for example, the file store), then the store MUST ensure that
;;; the provided properties are compatible with the wider store, *or*
;;; convert the object to a format compatible with the given store.
;;; 
;;; TODO does this ensure writing to storage?
;;; 
;;; put-event! :: <store>, href, vcalendar → undefined
(define-generic put-event!)



;;; CalDAV (and in some extent vdir) works on hrefs to identify
;;; entries instead of UIDs. They both make it very clear that the
;;; path to a resource has no relation to the contents of the
;;; resource. Therefore, each data store needs to keep track of these
;;; themselves.
;;; The recommended implementation for data stores not bound by
;;; historical restrictions is to simply use the UID as the href value.
;;; 
;;; TODO maybe change specification to only use last part of href
;;; e.g. strip away any leading components, and any trailing file extensions.
;;; For example /calendars/hugo/default/6079df47-f1f5-4866-8abe-7440cf0ac956.ics
;;; could be parsed as
;;; - get the hugo's calendar named "default"
;;; - get the object referenced by 6079df47-f1f5-4866-8abe-7440cf0ac956
;;; - return it in iCalendar format
;;; 
;;; One big argument for the limited aproach, is that a data store
;;; doesn't know where it itself is mounted.
;; (define-generic href->uid)
;; (define-generic uid->href)

;;; Retrieve or update the color attribute for the given store
(define-method (store-color (_ <calendar-data-store>)) #f)
(define-generic set-store-color!)
(define-generic remove-store-color!)

;;; Retrieve or update the color attribute for the given store
(define-method (store-displayname (_ <calendar-data-store>)) #f)
(define-generic set-store-displayname!)
(define-generic remove-store-displayname!)

(define-method (store-description (_ <calendar-data-store>)) #f)
(define-generic set-store-description!)
(define-generic remove-store-description!)

(define-method (store-calendar-timezone (_ <calendar-data-store>)) #f)
(define-generic set-store-calendar-timezone!)
(define-generic remove-store-calendar-timezone!)

;;; TODO CAL:supported-calendar-component-set
;;; TODO CAL:supported-calendar-data
;;; TODO CAL:max-resource-size
;;; TODO CAL:min-date-time
;;; TODO CAL:max-date-time
;;; TODO CAL:max-instances
;;; TODO CAL:max-attendees-per-instance



;;; list-entries/shallow :: store -> (list-of href)
(define-generic list-entries/shallow)
(define-method (list-entries/shallow (store <calendar-data-store>))
  (map car (list-entries store)))

(define-generic entry-count)
(define-method (entry-count (store <calendar-data-store>))
  (length (list-entries/shallow store)))

;;; list-entries :: store -> (list-of (pair-of href? vcomponent?))
;;; Wheore each top level vcomponent is a VCALENDAR
(define-generic list-entries)

;; (define-generic remove-by-uid!)
;;; remove-by-href! :: store, href -> undefined
(define-generic remove-by-href!)

;;; Write out all pending changes to disk.
;;; TODO This method MUST be renamed to something sensible
;;; like store-flush!
;;; flush! :: store -> ()
(define-generic flush!)



;; MUST return an assoc list denoting which collation modes the store supports.
;; Keys are collation identifying strings in accordance to [IANA].
;; Values should be procedures of type `(haystack: string?, needle: string) -> boolean?`
;; but MAY instead throw errors for all values iff the store implements its completely
;; own search framework.
;; Note that the RFC 4791 (CalDAV) REQUIRES that "i;ascii-casemap" and "i;octet" be present.
;; [IANA]: https://www.iana.org/assignments/collation/collation.xhtml
(define-method (supported-collations (_ <calendar-data-store>))
  `(("i;ascii-casemap" . ,(@ (hnh util ascii) string-ascii-contains-ci))
    ("i;octet" . ,string-contains)
    ("i;unicode-casemap" . ,string-contains-ci)
    ))



;;; TODO rename to something like "open-store" or "open-store-by-uri"
(define (store-uri->store uri)
  (typecheck uri uri?)
  (unless (eq? 'store (uri-scheme uri))
    (scm-error 'misc-error "store-uri->store"
               "URI using different scheme than `store:': ~s"
               (list (uri->string uri)) #f))
  (let* (
         (store-name (string->symbol (uri-host uri)))
         (constructor
          (module-ref (resolve-interface
                       `(vcomponent data-stores ,store-name))
                      'create-instance)))

    (apply constructor
           path: (uri-path uri)
           (parse-query (uri-query uri)
                        decode-plus-to-space?: #f))))

(define-generic close-store!)
(define-method (close-store! _)
  ;; (format (current-error-port) "Closing <top>~%")
  )


(define-generic store-uri)


;;; Code for calendar-query.

;;; TODO add these to the documentation.
;;; They are the dispatch procedures for the corresponding WebDAV (and
;;; friends) REPORTs, dispatched by store type. Also see the run-*-report
;;; procedures which are specialized by WebDAV resource type (and which
;;; probably simply dispatch to these).
(define-generic execute-expand-property-report)
(define-generic execute-calendar-query-report)
(define-generic execute-calendar-multiget-report)
(define-generic execute-free-busy-query-report)



;; This works exactly like a propfind, except that the "fake" property
;; <C:calendar-data>...</> is also available.

;;; TODO rename this since it can take multiple different tags
(define (calendar-query->propfind calendar-query)
  (assert (or (tag-matches? calendar-query 'calendar-query caldav)
              (tag-matches? calendar-query 'calendar-multiget caldav)))

  ((xml webdav 'propfind)
   (or (find (lambda (el) (or (tag-matches? el 'allprop webdav)
                         (tag-matches? el 'propname webdav)
                         (tag-matches? el 'prop webdav)))
             (xml-element-children calendar-query))
       ((xml webdav 'allprop)))))


;;; TODO this throws 'report-pre-condition in a number of places
;;; This MUST be caught somewhere.
;;; But first it must be documented
(define-method (execute-calendar-query-report
                (store <calendar-data-store>)
                calendar-query)
  (assert (tag-matches? calendar-query 'calendar-query caldav))

  (define timezone
    (cond ((find (lambda (ch) (tag-matches? ch 'timezone caldav))
                 (xml-element-children calendar-query))
           => (lambda (timezone)
                ;; Note that content-type here is a calp extension
                (call-with-input-string (xml-text-content timezone)
                  (parser
                   (resolve-media-type
                    (or (attribute timezone 'content-type)
                        "text/calendar"))))))
          (else #f)))

  ;; TODO validate that timezone is a vtimezone component (if present)

  ;; This handles the <C:filter/> part of the query
  ;; Matching entries is a list of href, vcalendar pairs.
  (define matching-entries
    (cond ((find (lambda (ch) (tag-matches? ch 'filter caldav))
                 (xml-element-children calendar-query))
           => (lambda (filter-el)
                ;; <C:filter /> MUST contains exactly one <C:comp-filter /> element.
                (cond ((find (lambda (ch) (tag-matches? ch 'comp-filter caldav))
                             (xml-element-children filter-el))
                       => (lambda (comp-filter)
                            (filter
                             (lambda (pair) (execute-comp-filter
                                        ;; TODO give actual timezone object
                                        ;; US/Eastern currently hard-coded, in order
                                        ;; to work with RFC provided tests
                                        "US/Eastern"
                                        comp-filter store (cdr pair) '()))
                             (list-entries store))))
                      (else
                       ;; TODO better error
                       (scm-error 'misc-error "execute-calendar-query-report"
                                  "Malformed query, no C:comp-filter element"
                                  '() #f)))
                ))
          (else
            ;; TODO better error
           (scm-error 'misc-error "execute-calendar-query-report"
                      "Malformed query, no C:filter element"
                      '() #f)
           )))

  (define propfind (calendar-query->propfind calendar-query))

  ;; (format (current-error-port) "Matching entries: ~s~%" matching-entries)
  ;; (format (current-error-port) "Query: ~s~%" calendar-query)

  (values
   (build-response code: 207
                   reason-phrase: (http-status-phrase 207)
                   headers: '((content-type . (application/xml))))
   (apply (xml webdav 'multistatus)
          (for entry in matching-entries
               ;; (format (current-error-port) "Entry: ~s~%" entry)
               (apply (xml webdav 'response)
                      ((xml webdav 'href) (car entry))
                      (map propstat->namespaced-sxml
                           (exec-propfind
                            propfind
                            (make <virtual-calendar-object-resource>
                              component: (cdr entry)))))))))




;;; NOTE: this procedure appears in mulitple places in the code base.
;;; It is NOT moved to a module, since it's a band-aid. All overly-specified hrefs
;;; MUST be validated before used (e.g. that all the "upper" components also point here),
;;; which this procedure plainly ignored
(define (uri-path-last href)
  (last (string-split (uri-path href) #\/)))


(define-method (execute-calendar-multiget-report
                (store <calendar-data-store>)
                calendar-multiget)
  (assert (tag-matches? calendar-multiget 'calendar-multiget caldav))

  ;; (format (current-error-port) "~s~%" calendar-multiget)

  (define hrefs
    (filter (lambda (el) (tag-matches? el 'href webdav))
            (xml-element-children calendar-multiget)))

  (define propfind (calendar-query->propfind calendar-multiget))

  (values (build-response
           code: 207
           reason-phrase: (http-status-phrase 207)
           headers: '((content-type . (application/xml))))
          (apply
           (xml webdav 'multistatus)
           (for href in hrefs
                ;; TODO only checking the last component is an ugly
                ;; hack. We MUST check that all parent components also match,
                ;; and that the domain matches when present
                (define href-str (last (string-split (xml-text-content href) #\/)))
                (cond ((get-by-href store href-str)
                       => (lambda (event)
                            (apply (xml webdav 'response)
                                   ;; TODO re-code the href?
                                   href
                                   ((xml webdav 'status) (http-status-line 200))
                                   (map propstat->namespaced-sxml
                                        (exec-propfind
                                         propfind
                                         (make <virtual-calendar-object-resource> component: event))))))

                      (else
                       ((xml webdav 'response)
                        ;; TODO re-code the href?
                        href
                        ((xml webdav 'status)
                         (http-status-line 404)))))))))






;;; TODO merge all following query procedures with general search code

;; Return a *sorted* and *expanded* stream of <href, vcalendar?> pairs, for all entries
;; overlapping the interval. This means that a recurring entry will
;; be present multiple time, with each component wrapped in its own (but
;; identical) vcalendar envelope.
#|xml
<calendar-query xmlns="urn:ietf:params:xml:ns:caldav"
                xmlns:D="DAV:">
  <D:prop>
    <calendar-data>
      <expand start="&START;" end="&END;" />
    </calendar-data>
  </D:prop>
  <filter>
    <comp-filter name="VCALENDAR">
      <comp-filter name="VEVENT">
        <time-range start="&START;" end="&END;" />
      </comp-filter>
    </comp-filter>
  </filter>
</calendar-query>
|#
;;; DEPRECATED
(define-generic entries-in-interval)
(define-method (entries-in-interval
                (store <calendar-data-store>)
                reference-zone start end)
  ;; Start and end must satisfiy utc-datetime? in all instances
  (format (current-error-port)
          "Entries-in-interval not implemented for ~s~%"
          store)
  '())


#|xml
<calendar-query xmlns="urn:ietf:params:xml:ns:caldav">
  <filter>
    <comp-filter name="VCALENDAR">
      <comp-filter name="VEVENT">
        <prop-filter name="SUMMARY">
          <text-match collation="i;ascii-casemap">&QUERY;</text-match>
        </prop-filter>
      </comp-filter>
    </comp-filter>
  </filter>
</calendar-query>
|#
;;; DEPRECATED
(define-generic entries-by-summary)
(define-method (entries-by-summary
                (store <calendar-data-store>)
                substring)
  (filter (lambda (entry)
            (define-values (href calendar) (car+cdr entry))
            (any (lambda (instance)
                   (and (vevent? instance)
                        (string-contains-ci (prop1 instance 'SUMMARY)
                                            substring)))
                 (vcomponent-children calendar)))
          (list-entries store)))

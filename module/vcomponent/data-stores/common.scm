;;; These are the common methods for implementing all calendar data stores.
;;; Each data store type provides a way to store the data (such as a
;;; file, a database, ...), while each data store instance is a specific
;;; calendar.

;;; TODO move this module to (vcomponent data-stores)

(define-module (vcomponent data-stores common)
  :use-module (oop goops)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util optional)
  :use-module (hnh util lens)
  :use-module (srfi srfi-1)
  :use-module (vcomponent)
  :use-module (sxml namespaced)
  :use-module ((calp namespaces) :select (caldav))
  :use-module (sxml namespaced util)
  :use-module (web uri)
  :use-module ((web query) :select (parse-query))
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

           store-color set-store-color! remove-store-color!
           store-displayname set-store-displayname! remove-store-displayname!
           store-description set-store-description! remove-store-description!
           store-calendar-timezone set-store-calendar-timezone! remove-store-calendar-timezone!

           store-uri->store

           store-uri
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

;;; list-entries/shallow :: store -> href
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
;;; This method MUST be renamed to something sensible
;;; flush! :: store -> ()
(define-generic flush!)

;;; caldav-filter :: store -> filter-xml -> (list-of vcalendar)
;;; TODO
;; (define-generic caldav-filter)

(define (octet-string-contains haystack needle)
  'TODO
  )

(define collations
  (make-parameter
   `(;("i;ascii-casemap" . ,ascii-casemap)
     ;("i;octet" . ,octet)
     ;("i;unicode-casemap" . ,unicode-casemap)
     )
   ))

(define (run-text-match text-match value)
  (typecheck text-match xml-element?)
  ;; TODO this doesn't work, since we already converted the source
  ;; strings into abstract data types. We need to un-parse them maybe
  (typecheck value string?)
  (define collation (or (attribute text-match 'collation) "i;ascii-casemap"))
  (define negate (cond ((attribute text-match 'negate-condition)
                        => (lambda (n) (string=? n "yes")))
                       (else #f)))

  (xml-text-content text-match)

  )

(define-method (caldav-filter (store <calendar-data-store>) query)
  (typecheck query xml-element?)
  (filter (lambda (ev) (run-caldav-filter ev query))
          (list-entries store)))



(define (store-uri->store uri)
  (typecheck uri uri?)
  (unless (eq? 'store (uri-scheme uri))
    (scm-error 'misc-error "store-uri->store"
               "URI using different scheme than `store:': ~s"
               (list (uri->string uri)) #f))
  (let* (
         ;; If uri is given as `store://<format>?<args>`, then format
         ;; ends up in the host field, with path being the empty
         ;; string. However `store:<format>?<args>` places format in
         ;; the path, with the host being `#f`.
         (store-name (string->symbol (or (uri-host uri)
                                         (uri-path uri))))
         (constructor
          (module-ref (resolve-interface
                       `(vcomponent data-stores ,store-name))
                      'create-instance)))

    (apply constructor (parse-query (uri-query uri)
                                    decode-plus-to-space?: #f))))


;;; TODO document
(define-generic store-uri)

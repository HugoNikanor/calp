(define-module (calp webdav resource calendar collection)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (oop goops)
  :use-module (calp webdav resource)
  :use-module (calp webdav href)
  :use-module (calp webdav property)
  :use-module (calp webdav propfind)
  :use-module ((vcomponent formats ical) :prefix #{ics:}#)
  :use-module ((vcomponent) :prefix vcs-)
  :use-module ((vcomponent base)
               :select (type prop vcomponent))

  :use-module (web request)
  :use-module (web uri)

  :use-module ((calp namespaces) :select (webdav caldav))
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (ice-9 hash-table)

  :use-module (hnh util)

  :use-module (calp webdav resource calendar object)
  ;; propfind-most-live-properties propfind-all-dead-properties propname uri-path request-uri type
  :export (<calendar-collection-resource>
           caldav-properties
           calendar-collection-resource?)
  )

;;; Resoruces containing calendar components
(define-class <calendar-collection-resource> (<resource>)
  ;; TODO typecheck
  (description init-value: #f
               accessor: description)
  (data-store getter: data-store
              init-keyword: store:))


(define-method (collection? (_ <calendar-collection-resource>))
  #t)



(define-method (children (this <calendar-collection-resource>))
  (map (lambda (ev)
         (make <calendar-object-resource>
           name: (prop ev 'UID)
           component: ev))
       (vcs-children this)))

(define (calendar-collection-resource? x)
 (is-a? x <calendar-collection-resource>))


(define-method (base-timezone <calendar-collection-resource>)
  ;; (zoneinfo->vtimezone '() "Europe/Stockholm" 'ev)
  (vcomponent type: 'VTIMEZONE)
  )



(define-method (live-properties (self <calendar-collection-resource>))
  (append (next-method)
          (map (lambda (pair) ((xml caldav (car pair)) (cdr pair)))
               caldav-properties)))




(define-method (displayname (self <calendar-collection-resource>))
  (propstat 200
            (list ((xml webdav 'displayname)
                   (prop (content self) 'displayname)))))


(define-method (resourcetype (self <calendar-collection-resource>))
  (propstat 200
            (list ((xml webdav 'resourcetype)
                   ((xml caldav 'calendar))))))

;;; CALDAV Properties

(define-method (calendar-description (self <calendar-collection-resource>))
  (cond ((description self)
         => (lambda (it)
              (propstat 200
                        (list ((xml caldav 'calendar-description '((xml:lang "en")))
                               it)))))
        (else
         (propstat 404 (list ((xml caldav 'calendar-description)))))))

(define-method (calendar-timezone (self <calendar-collection-resource>))
  (propstat 200
            (list
             ((xml caldav 'calendar-description)
              (call-with-output-string
                (lambda (port)
                  (ics:serialize (base-timezone self) port)))))))

(define-method (supported-calendar-component-set (self <calendar-collection-resource>))
  (propstat 200
            (list ((xml caldav 'supported-calendar-component-set)
                   ((xml caldav 'comp
                         '((name "VEVENT"))))))))

(define-method (supported-calendar-data (self <calendar-collection-resource>))
  (propstat 200
   (list
    ((xml caldav 'supported-calendar-data)
     (map (lambda (content-type)
            ((xml caldav 'calendar-data
                  '((content-type ,content-type)
                    (version "2.0")))))
          '("text/calendar"
            "application/calendar+xml"))))))



;; (define-method (max-resource-size (self <calendar-collection-resource>))
;;   )

;; (define-method (min-date-time ))
;; (define-method (max-date-time ))
;; (define-method (max-instances ))
;; (define-method (max-attendees-per-instance ))

(define-method (supported-collation-set (self <calendar-collection-resource>))
  (propstat 200
            (list ((xml caldav 'supported-collation-set)
                   (map (lambda (cs) ((xml caldav 'supported-collation) cs))
                        `(;; Required by CalDAV
                          "i;ascii-casemap"
                          "i;octet"
                          ;; Added (RFC 5051))
                          "i;unicode-casemap"))))))



(define caldav-properties
  `((calendar-description . ,calendar-description)
    (calendar-timezone . ,calendar-timezone)
    (supported-calendar-component-set . ,supported-calendar-component-set)
    (supported-calendar-data . ,supported-calendar-data)
    (supported-collation-set . ,supported-collation-set)
    ;; (max-resource-size . ,max-resource-size)
    ;; (min-date-time . ,min-date-time)
    ;; (max-date-time . ,max-date-time)
    ;; (max-instances . ,max-instances)
    ;; (max-attendees-per-instance . ,max-attendees-per-instance)
    ))

;;; Reports

(define-method (supported-reports* (this <calendar-collection-resource>))
  (append (next-method)
          (list
           ;; Required for ACL, but not for CalDAV
           ;; (xml webdav 'version-tree)
           ;; Optional for ACL, but REQUIRED for CalDAV
           ((xml webdav 'expand-property) expand-property)
           ;; REQUIRED by CalDAV
           ((xml caldav 'calendar-query) calendar-query)
           ((xml caldav 'calendar-multiget) calendar-multiget)
           ((xml caldav 'free-busy-report) free-busy-report)
           )))


(define-method (calendar-query (this <calendar-collection-resource>) headers body)
  ;; Request body MUST be a caldav:calendar-query
  ;; Request MAY include a depth header, default = 0
  ;; Respnose-body MUST be a dav:multistatus
  ;; Responseb body MUST contain DAV:respons element for each iCalendar object that matched the search filter

  (let ((allprop  (find-element (xml webdav 'allprop)  (cdr body)))
        (propname (find-element (xml webdav 'propname) (cdr body)))
        (prop     (find-element (xml webdav 'prop)     (cdr body)))
        (filter   (find-element (xml caldav 'filter)   (cdr body)))
        (timezone (find-element (xml caldav 'timezone) (cdr body))))
    (when (< 1 (count identity (list allprop propname prop)))
      (throw 'http 400 "allprop, propname, and prop are mutually exclusive"))

    (unless filter
      (throw 'http 400 "filter required"))


    #;
    (when timezone
    (case (assoc-ref (attributes timezone) 'content-type)
    ((application/calendar+xml)
    (xcs:serialize default-timezone))
    ;; ((application/calendar+json))
    (else ; includes text/calendar
    (ics:serialieze default-timezone)
    )))

    (let ((resources (select-components-by-comp-filter this comp-filter)))
      (apply (xml webdav 'multistatus)
             (for (href . resource) in resources
                  (apply (xml webdav 'response)
                         ((xml webdav 'href) (href->string href))
                         (map propstat->namespaced-sxml
                                (merge-propstats
                                 (cond (allprop
                                        (append (propfind-most-live-properties resource)
                                                (propfind-all-dead-properties resource)))
                                       (propname
                                        (list (propstat
                                               200
                                               (map (compose (lambda (x) ((xml x))) car)
                                                    (append (dead-properties resource)
                                                            (live-properties resource))))))
                                       (prop
                                        (map (lambda (prop) (get-property resource prop))
                                             prop)))))))))))




(define-method (expand-property (this <calendar-collection-resource>) request body))

(define-method (free-busy-report (this <calendar-collection-resource>) request body))

(define-method (calendar-multiget (this <calendar-collection-resource>) request body)
  (define base-href (-> request request-uri uri-path href->string))
  (let ((allprop  (find-children ((xml webdav 'allprop))  (cdr body)))
        (propname (find-children ((xml webdav 'propname)) (cdr body)))
        (prop     (find-children ((xml webdav 'prop))     (cdr body)))
        (hrefs    (find-children ((xml webdav 'href))     (cdr body))))
    (when (< 1 (count identity (list allprop propname prop)))
      (throw 'http 400 "allprop, propname, and prop are mutually exclusive"))
    (when (null? hrefs)
      (throw 'http 400 "At least one href is required"))

    ;; (assert (memv href hrefs))

    (let ((resources
           (for href in hrefs
                (cons href
                      (lookup-resource
                       this
                       (href-relative base-href href))))))
      (apply (xml webdav 'multistatus)
             (for (href . resource) in resources
                  (apply (xml webdav 'response)
                         ((xml webdav 'href) (href->string href))
                         (cond (resource
                                (cond (allprop
                                       (append (propfind-most-live-properties resource)
                                               (propfind-all-dead-properties resource)))
                                      (propname
                                       (list (propstat
                                              200
                                              ;; car to get tagname, list to construct a valid xml element
                                              (map (compose (lambda (x) ((xml x))) car)
                                                   (append
                                                    (dead-properties resource)
                                                    (live-properties resource))))))
                                      (prop
                                       (propfind-selected-properties
                                        resource
                                        ;; TODO???
                                        (map car (cdr prop))))))
                               (else
                                ((xml webdav 'status)
                                 (http-status-line 404))))))))))




(define-method (select-components-by-comp-filter (this <calendar-collection-resource>) comp-filter)
  )


;;; TODO
(define (overlaps? a b)
  #t)

(define (comp-filter scope filter)
  ;; CaldDAV 9.7.1
  (or (and (null? (children filter))
           (eq? (attribute filter 'name)
                (type scope)))
      (and (find-element (xml caldav 'is-not-defined)
                         (children filter))
           (not
            (find (lambda (el) (eq? (type el) (attribute filter 'name)))
                  (children scope))))
      (and (cond ((find-element (xml caldav 'time-range)
                                (children filter))
                  => (lambda (range)
                       (overlaps? scope range)))
                 (else #f))
           (every (lambda (filt) (comp-filter scope filt)) (children filter)))
      (every (lambda (filt) (comp-filter scope filt)) (children filter))))

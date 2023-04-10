(define-module (calp webdav resource calendar)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (oop goops)
  :use-module (vcomponent)
  :use-module (datetime)
  :use-module (sxml namespaced)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :use-module (ice-9 hash-table)
  :use-module ((vcomponent formats ical) :prefix #{ics:}#)
  :export (<calendar-resource>
           calendar-resource?
           content
           caldav-properties)
  )

;;; Resoruces containing calendar components
(define-class <calendar-resoruce> (<resource>)
  (description init-value: #f
               accessor: description)
  (content init-value: (make-vcomponent 'VIRTUAL)
           accessor: content))

(define (calendar-resource? x)
  (is-a? x <calendar-resource>))

(define-method (live-properties (self <calendar-resource>))
  (append (next-method)
          (map (lambda (pair) (cons (xml caldav (car pair)) (cdr pair)))
               caldav-properties)))

(define-method (creationdate (self <calendar-resource>))
  (propstat 200
            `((,(xml webdav 'creationdate)
               (-> (content self)
                   (prop 'CREATED)
                   ;; TODO timezone
                   (datetime->string "~Y-~m-~dT~H:~M:~SZ"))))))

(define-method (displayname (self <calendar-resource>))
  (propstat 200
            `((,(xml webdav 'displayname)
               ,(prop (content self) 'displayname)))))


(define-method (getcontentlength (self <calendar-resoruce>))
  ;; TODO which representation should be choosen to calculate length?
  (propstat 501 `((,(xml webdav 'getcontentlength)))))

(define-method (getcontenttyype (self <calendar-resource>))
  ;; TODO different representations
  (propstat 200 `((,(xml webdav 'getcontentlength)
                   "text/calendar"))))


(define-method (getlastmodified (self <calendar-resource>))
  (propstat 200
            `((,(xml webdav 'getlastmodified)
               (string->datetime (prop (content self) 'LAST-MODIFIED)
                                 "~Y~m~dT~H~M~S")))))


(define-method (resourcetype (self <calendar-resource>))
  (propstat 200
   `((,(xml webdav 'resourcetype)
      (,(xml caldav 'calendar))))))

;;; CALDAV Properties

;; NOT in allprop
(define-method (calendar-description (self <calendar-resource>))
  (cond ((description self)
         => (lambda (it)
              (propstat 200
                        (list (list (xml caldav 'calendar-description (alist->hashq-table '((xml:lang . "en"))))
                                    it)))))
        (else
         (propstat 404 (list (list (xml caldav 'calendar-description)))))))

;; NOT in allprop
(define-method (calendar-timezone (self <calendar-resource>))
  (propstat 200
            (list
             (list (xml caldav 'calendar-description)
                   ;; TODO serialize, base-timezone
                   (ics:serialize (base-timezone (content self)))))))

;; NOT in allprop
(define-method (supported-calendar-component-set (self <calendar-resource>))
  (propstat 200
            `((,(xml caldav 'supported-calendar-component-set)
               (,(xml caldav 'comp
                      (alist->hashq-table '((name . "VEVENT")))))))))

(define-method (supported-calendar-data (self <calendar-resource>))
  (propstat 200
   (list
    (list
     (xml caldav 'supported-calendar-data)
     (map (lambda (content-type)
            (list (xml caldav 'calendar-data
                       (alist->hashq-table
                        '((content-type . ,content-type)
                          (version . "2.0"))))))
          '("text/calendar" "application/calendar+xml"))))))

;; (define-method (max-resource-size (self <calendar-resource>))
;;   )

;; (define-method (min-date-time ))
;; (define-method (max-date-time ))
;; (define-method (max-instances ))
;; (define-method (max-attendees-per-instance ))

(define caldav-properties
  `((calendar-description . ,calendar-description)
    (calendar-timezone . ,calendar-timezone)
    (supported-calendar-component-set . ,supported-calendar-component-set)
    (supported-calendar-data . ,supported-calendar-data)
    ;; (max-resource-size . ,max-resource-size)
    ;; (min-date-time . ,min-date-time)
    ;; (max-date-time . ,max-date-time)
    ;; (max-instances . ,max-instances)
    ;; (max-attendees-per-instance . ,max-attendees-per-instance)
    ))



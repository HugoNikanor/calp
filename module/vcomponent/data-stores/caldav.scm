;;; This is a badly written proof of concept for a data-store, which
;;; is a CalDAV client. It doesn't work at all.

;;; TODO this is a very early draft, and basically need to be
;;; re-written from scratch to have any hope of ever working.

(define-module (vcomponent data-stores caldav)
  )

(use-modules (srfi srfi-71)
             (srfi srfi-88)
             (rnrs bytevectors)
             (rnrs io ports)
             ((ice-9 binary-ports) :select (call-with-output-bytevector))
             (web request)
             (web response)
             (web client)
             (web uri)
             ;; (web http)                 ;
             (sxml simple)
             (oop goops)
             (vcomponent data-stores common)
             ((hnh util) :select (->))
             (web http dav)
             )



(define-class <caldav-data-store> (<calendar-data-store>)
  (host init-keyword: host:
        getter: host)
  (user init-keyword: user:
        getter: user)
  (calendar-path init-keyword: calendar-path:
                 accessor: calendar-path)
  (password init-keyword: password:
            getter: store-password))


(define local-uri
  (case-lambda ((this path)
                (build-uri 'https
                           host: (host this)
                           path: path))
               ((this)
                (build-uri 'https
                           host: (host this)
                           path: (calendar-path this)))))


(define* (make-caldav-store key: host user path password)
  (define store
    (make <caldav-data-store>
      host: host
      user: user
      password: (string->symbol password)
      calendar-path: path))


  (let* ((principal-path
           (get-principal (local-uri store "/")
                          password: (store-password store)))
         (calendar-home-set
           (get-calendar-home-set (local-uri store principal-path)
                                  password: (store-password store)))
         (calendar-paths
           (get-calendar-paths (local-uri store calendar-home-set)
                               password: (store-password store))))
    (set! (calendar-path store)
      (car calendar-paths)))

  store)

(define-method (write (this <caldav-data-store>) port)
  (write `(make-caldav-store host: ,(host this)
                             user: ,(user this)
                             calendar-path: ,(calendar-path this)
                             password: ,(store-password this))
         port))

(define store
 (make-caldav-store host: "dav.fruux.com"
                    user: "a3298201184"
                    password: "YjMyOTc0NjUwMDk6YXRhc3llanY2MGtu"))

#;
(define-method (calendar-base (this <caldav-data-store>))
  (build-uri 'https
            host: (host this)
            path: (calendar-path this)))


;; (define-method (get-all (this <caldav-data-store>))
;;   )

(define-method (get-by-uid (this <caldav-data-store>)
                           (uid <string>))
  (let ((uids
         (dav (local-uri this)
              method: 'REPORT
              authorization: `(Basic ,(store-password this))
              depth: 1
              body:
              `(c:calendar-query
                (@ (xmlns:c ,caldav))
                (d:prop (@ (xmlns:d "DAV:"))
                        (d:getetag)
                        #; (c:calendar-data)
                        )
                (c:filter
                 (c:comp-filter
                  (@ (name "VCALENDAR"))
                  (c:comp-filter
                   (@ (name "VEVENT"))
                   (c:prop-filter
                    (@ (name "UID"))
                    (c:text-match (@ (collation "i;octet"))
                                  ,uid)))))))))
    uids))


(define-method (search (this <caldav-data-store>)
                       (filter <pair>))
  (let ((uids
         (dav (local-uri this)
              method: 'REPORT
              authorization: `(Basic ,(store-password this))
              depth: 1
              body:
              `(c:calendar-query
                (@ (xmlns:c ,caldav))
                (d:prop (@ (xmlns:d "DAV:"))
                        (d:getetag)
                        (c:calendar-data
                         (c:comp (@ (name "VCALENDAR"))
                                 (c:prop (@ (name "PRODID")))))
                        #; (c:calendar-data)
                        )
                ,filter))))
    uids))

(define-method (search (this <caldav-data-store>)
                       (filter <string>)
                       (field <string>))
  (search store
          `(c:filter
            (c:comp-filter
             (@ (name "VCALENDAR"))
             (c:comp-filter
              (@ (name "VEVENT"))
              (c:prop-filter
               (@ (name ,field))
               (c:text-match (@ (collation "i;octet"))
                             ,filter)))))))



(define-method (list-calendars (this <caldav-data-store>))
  )




;; (get-principal) ; => "/principals/uid/a3298201184/"

(get-calendar-home-set "/principals/uid/a3298201184/")
;; => "/calendars/a3298201184/"

(get-calendar-paths "/calendars/a3298201184/")
;; => ("/calendars/a3298201184/b85ba2e9-18aa-4451-91bb-b52da930e977/")



(define user "a3298201184")
(define calendar "b85ba2e9-18aa-4451-91bb-b52da930e977")
(define password (string->symbol "YjMyOTc0NjUwMDk6YXRhc3llanY2MGtu"))
(define auth `(Basic ,password))






(define uri
  (build-uri 'https
             host: "dav.fruux.com"
             path: "/calendars/a3298201184/b85ba2e9-18aa-4451-91bb-b52da930e977/ff95c36c-6ae9-4aa0-b08f-c52d84bf4f26.ics"))

(define (get-sample)
 (define-values (response body)
   (dav uri
        method: 'GET
        authorization: auth))
 'done)




(define (propfind-sample)
 (define-values (response body)
   (dav uri
        method: 'PROPFIND
        authorization: auth
        body:
        `(C:supported-collation-set (@ (xmlns:C ,caldav)))))
 'done)

(define (report-sample)
 (define-values (response body)
   (dav uri
        method: 'REPORT
        authorization: auth
        body:
        `(C:calendar-query
          (@ (xmlns:C ,caldav))
          (D:prop (@ (xmlns:D "DAV:"))
                  (D:getetac)
                  (C:calendar-data))
          (C:filter
           (C:comp-filter
            (@ (name "VCALENDAR"))
            (C:comp-filter
             (@ (name "VEVENT"))
             (C:prop-filter (@ (name "UID"))
                            (C:text-match (@ (collation "i;utf-8"))
                                          "Admittansen"))))))))
 'done)






;; (define (add)
;;  ;; add new event
;;  (http-request 'PUT
;;                path: "/path-on-server/<filename>.ics"
;;                headers:
;;                `((if-none-match "*")
;;                  (content-type "text/calendar"))
;;                body: (ics:serialize event-with-wrapping-calendar)
;;                ))


;; (define (get-by-time-range)
;;   (http-request 'REPORT
;;                 path: "/calendar/<calendar-name>"
;;                 body:
;;                 ;; See RFC 4791 7.8.1
;;                 `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
;;                         (C:calendar-query
;;                          (@ (xmlns:D "DAV:")
;;                             (xmlns:C "urn:ietf:params:xml:ns:caldav"))
;;                          (D:prop
;;                           (D:getetag)
;;                           (C:calendar-data
;;                            (C:comp
;;                             (@ (name "VCALENDAR"))
;;                             (C:prop (@ (name "VERSION")))
;;                             (C:prop (@ name "VEVENT")
;;                                     (C:prop (@ (name "SUMMARY")))
;;                                     ...))))
;;                          (C:filter
;;                           (C:comp-filter
;;                            (@ (name "VCALENDAR"))
;;                            (C:comp-filter
;;                             (@ (name "VEVENT"))
;;                             (C:time-range
;;                              (@ (start ,(datetime->string
;;                                          start
;;                                          "~Y~m~dT~H~M~S~Z"))
;;                                 (end ,(datetime->string
;;                                        end
;;                                        "~Y~m~dT~H~M~S~Z")))))))))))

;; (define (get-modified-after date)
;;   (http-request 'REPORT
;;                 path: "/<collection>"
;;                 body:
;;                 `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
;;                         (C:calendar-query
;;                          (@ (xmlns:D "DAV:")
;;                             (xmlns:C "urn:ietf:params:xml:ns:caldav"))
;;                          (D:prop (D:getetag))
;;                          (C:filter
;;                           (C:comp-filter
;;                            (@ (name "VCALENDAR"))
;;                            (C:comp-filter
;;                             (@ (name "VEVENT"))
;;                             (C:prop-filter
;;                              (@ (name "DTSTAMP"))
;;                              (C:time-range
;;                               (@ (start ,(datetime->string date "~Y~m~dT~H~M~S~Z")))))))))))

;;   (http-request 'REPORT
;;                 path: "/<collection>"
;;                 body:
;;                 `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
;;                         (C:calendar-query
;;                          (@ (xmlns:D "DAV:")
;;                             (xmlns:C "urn:ietf:params:xml:ns:caldav"))
;;                          (D:prop (D:getetag))
;;                          (C:filter
;;                           (C:comp-filter
;;                            (@ (name "VCALENDAR"))
;;                            (C:comp-filter
;;                             (@ (name "VEVENT"))
;;                             (C:prop-filter (@ (name "DTSTAMP"))
;;                                            (C:is-not-defined))))))))
;;   )





;;; Curl considered due to occansional bugs in Guile's built in HTTP library.
;; (use-modules (curl))
;; (define c (curl-easy-init))
;; (curl-easy-setopt c 'url "https://hornquist.se")

;; (curl-easy-perform handle)

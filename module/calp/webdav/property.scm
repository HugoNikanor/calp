(define-module (calp webdav property)
  :use-module (sxml namespaced)
  :use-module (web http status-codes)
  :use-module ((srfi srfi-1) :select (concatenate find))
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (calp namespaces)
  :export (make-propstat
           propstat?
           propstat-status-code
           propstat-property
           propstat-error
           propstat-response-description

           propstat

           merge-propstats
           propstat-200?
           ;; propstat->sxml
           propstat->namespaced-sxml
           ))

;;; Commentary:
;;; Code:


;; Maps directly to [WEBDAV]'s propstat objects. This is just a simpler interface in the code.

(define-record-type <propstat>
  (make-propstat status prop error responsedescription)
  propstat?
  ;; An http status code indicating if this property is present
  (status propstat-status-code)
  ;; A list of namespaced sxml elements, such that they could all be
  ;; directly inserted as the children of <DAV::prop/>
  ;; @example
  ;; `((,(xml ns tag) "Content"))
  ;; @end example
  (prop propstat-property)

  ;; See [WEBCAL] propstat XML element
  (error propstat-error)
  (responsedescription propstat-response-description))

(define* (propstat code prop key: error responsedescription)
  (make-propstat code prop error responsedescription))

;; Query a given dead property from the given resource
;; property should be a xml-element item
;; (define (propfind-selected-property resource property)
;;   (cond ((get-dead-property resource property)
;;          => (lambda (it) (propstat 200 (list it))))
;;         (else (propstat 404 (list (list property))))))
;; Takes a list of <propstat> items, finds all where status, error, and
;; responsedescription are all equal, and merges the prop tags of all those.
;; Returns a new list of <propstat> items
(define (merge-propstats propstats)
  (map (lambda (group)
         (define-values (code error desc) (unlist (car group)))
         (make-propstat code
                        (concatenate
                         (map propstat-property (cdr group)))
                        error desc))
       (group-by (lambda (propstat)
                   (list (propstat-status-code propstat)
                         (propstat-error propstat )
                         (propstat-response-description propstat)))
                 propstats)))

(define (propstat-200? prop)
  (= 200 (propstat-status-code prop)))


;; (define (propstat->sxml propstat)
;;   `(d:propstat (d:prop ,(propstat-property propstat))
;;                (d:status ,(http-status-line (propstat-status-code propstat)))
;;                ,@(awhen (propstat-error propstat)
;;                         `((d:error ,it)))
;;                ,@(awhen (propstat-response-description propstat)
;;                         `((d:responsedescription ,it)))))

(define (propstat->namespaced-sxml propstat)
  `(,(xml webdav 'propstat)
    (,(xml webdav 'prop) ,@(propstat-property propstat))
    (,(xml webdav 'status) ,(http-status-line (propstat-status-code propstat)))
    ,@(awhen (propstat-error propstat)
             `((,(xml webdav 'error) ,it)))
    ,@(awhen (propstat-response-description propstat)
             `((,(xml webdav 'responsedescription) ,it)))))

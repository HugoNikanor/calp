(define-module (calp webdav property)
  :use-module (sxml namespaced)
  :use-module (web http status-codes)
  :use-module ((srfi srfi-1) :select (concatenate find))
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (calp namespaces)
  :export (propstat?
           propstat-status-code          propstat-status-code*
           propstat-property             propstat-property*
           propstat-error                propstat-error*
           propstat-response-description propstat-response-description*

           propstat

           merge-propstats
           propstat-200?
           ;; propstat->sxml
           propstat->namespaced-sxml

           make-live-property
           live-property?
           property-getter
           property-setter-generator
           property-remover-generator

           ))

;;; Commentary:
;;; Code:


;; Maps directly to [WEBDAV]'s propstat objects. This is just a simpler interface in the code.

(define-type (propstat
              constructor: (lambda (constructor typecheck)
                             (lambda* (code prop key: error responsedescription)
                               (typecheck code prop error responsedescription)
                               (constructor code prop error responsedescription)
                               )))
  ;; An http status code indicating if this property is present
  (propstat-status-code type: integer?)

  ;; A list of namespaced sxml elements, such that they could all be
  ;; directly inserted as the children of <DAV::prop/>
  ;; @example
  ;; (list ((xml ns tag) "Content"))
  ;; @end example
  (propstat-property type: (list-of xml-element?))

  ;; See [WEBCAL] propstat XML element
  (propstat-error keyword: error)

  (propstat-response-description
   ;; keyword: responsedescription
   type: (or false? string?)))


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
         (propstat code
                   (concatenate
                    (map propstat-property (cdr group)))
                   error: error responsedescription: desc))
       (group-by (lambda (propstat)
                   (list (propstat-status-code propstat)
                         (propstat-error propstat)
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
  (apply (xml webdav 'propstat)
         (append

          (list
           (apply (xml webdav 'prop) (propstat-property propstat))
           ((xml webdav 'status) (http-status-line (propstat-status-code propstat))))

          (awhen (propstat-error propstat)
                 (list ((xml webdav 'error) it)))

          (awhen (propstat-response-description propstat)
                 (list ((xml webdav 'responsedescription) it))))))


(define-type (live-property)
  (property-getter            keyword: getter  type: procedure?)
  (property-setter-generator  keyword: setter  type: procedure?)
  (property-remover-generator keyword: remover type: procedure?))


(define* (make-live-property getter setter-generator remover-generator)
  (live-property getter: getter
                 setter: setter-generator
                 remover: remover-generator))

(define-module (calp webdav propfind)
  :use-module (calp webdav property)
  :use-module (calp webdav resource)
  :use-module ((calp webdav resource base) :select (resource?))
  :use-module (calp namespaces)
  :use-module (srfi srfi-1)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((hnh util) :select (->))
  :use-module ((hnh util table) :select (table))
  :use-module (hnh util type)
  :export (propfind-selected-properties
           propfind-all-live-properties
           propfind-most-live-properties
           propfind-all-dead-properties

           parse-propfind
           ))

;;; Commentary:
;;; Procedures for the WebDav PROPFIND method
;;; Code:

;; Properties should be a list of xml-tag-elements
;; return a list of propstat elements
;; work for both dead and alive objects
(define (propfind-selected-properties resource properties)
  (typecheck resource resource?)
  (typecheck properties (list-of xml-element?))
  (map (lambda (el) (get-property resource el))
       properties))


;; (define-method (supported-properties (self <resource>))
;;   (map (lambda (v) (cons webdav v))
;;        `()))

;; Returns a list of <propstat> objects.
(define (propfind-all-live-properties resource)
  (typecheck resource resource?)
  (map (lambda (p) ((property-getter (cdr p)) resource))
       (live-properties resource)))

;; Returns a list of <propstat> objects.
;; The list being the live properties defined by [WEBDAV]
(define (propfind-most-live-properties resource)
  (typecheck resource resource?)
  (map (lambda (p) ((property-getter (cdr p)) resource))
       webdav-properties))

;; Returns a list of <propstat> objects.
;; All "dead" properties on resource.
(define (propfind-all-dead-properties resource)
  (typecheck resource resource?)
  (map (lambda (v) (propstat 200 (list v)))
       (dead-properties resource)))





;; Takes a propfind xml element (tree), and a webdav resource object.
;; Returns a list of <propstat> objects.
(define (parse-propfind sxml resource)
  (typecheck sxml xml-element?)
  (typecheck resource resource?)

  (let ((propname (find-child ((xml webdav 'propname)) (xml-element-children sxml)))
        (allprop  (find-child ((xml webdav 'allprop))  (xml-element-children sxml)))
        (include  (find-child ((xml webdav 'include))  (xml-element-children sxml)))
        (prop     (find-child ((xml webdav 'prop))     (xml-element-children sxml))))

    (merge-propstats
     (cond ((and allprop include)
            ;; Return "all" properties + those noted by <include/>
            (append (propfind-most-live-properties resource)
                    (propfind-all-dead-properties resource)
                    (propfind-selected-properties
                     resource
                     (xml-element-children include))))

           (allprop
            ;; Return "all" properties
            (append (propfind-most-live-properties resource)
                    (propfind-all-dead-properties resource)))

           (propname
            ;; Return the list of available properties
            ;; each entry is an xml element, with no content
            (list (propstat
                   200
                   (append
                    (map (lambda (el) (-> el (children '()) (properties (table))))
                     (dead-properties resource))
                    (map car (live-properties resource))))))

           (prop
            ;; Return the properties listed
            (propfind-selected-properties
             resource
             (xml-element-children prop)))

           (else
            (scm-error 'bad-request "parse-propfind"
                       "Invalid search query ~s" (list sxml) (list sxml)))))))

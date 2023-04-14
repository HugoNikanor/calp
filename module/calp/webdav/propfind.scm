(define-module (calp webdav propfind)
  :use-module (calp webdav property)
  :use-module (calp webdav resource)
  :use-module (calp namespaces)
  :use-module (srfi srfi-1)
  :use-module (sxml namespaced)
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
  (map (lambda (el) (get-property resource el))
       properties))


;; (define-method (supported-properties (self <resource>))
;;   (map (lambda (v) (cons webdav v))
;;        `()))

;; Returns a list of <propstat> objects.
(define (propfind-all-live-properties resource)
  (map (lambda (p) ((cdr p) resource))
       (live-properties resource)))

;; Returns a list of <propstat> objects.
;; The list being the live properties defined by [WEBDAV]
(define (propfind-most-live-properties resource)
  (map (lambda (p) ((property-getter (cdr p)) resource))
       webdav-properties))

;; Returns a list of <propstat> objects.
;; All "dead" properties on resource.
(define (propfind-all-dead-properties resource)
  (map (lambda (v) (propstat 200 (list v)))
       (dead-properties resource)))





(define (find-element target list)
  (define target* (xml-element-hash-key target))
  (find (lambda (x) (and (list? x)
                    (not (null? x))
                    (xml-element? (car x))
                    (equal? target* (xml-element-hash-key (car x)))))
        list))

;; Takes a propfind xml element (tree), and a webdav resource object.
;; Returns a list of <propstat> objects.
(define (parse-propfind sxml resource)
  ;; (assert (list? sxml))
  ;; (assert (not (null? sxml)))
  ;; (assert eq? 'd:propfid (car sxml))
  (let ((propname (find-element (xml webdav 'propname) (cdr sxml)))
        (allprop  (find-element (xml webdav 'allprop)  (cdr sxml)))
        (include  (find-element (xml webdav 'include)  (cdr sxml)))
        (prop     (find-element (xml webdav 'prop)     (cdr sxml))))
    (merge-propstats
     (cond ((and allprop include)
            ;; Return "all" properties + those noted by <include/>
            (append (propfind-most-live-properties resource)
                    (propfind-all-dead-properties resource)
                    (propfind-selected-properties
                     resource
                     (map car (cdr include)))))
           (allprop
            ;; Return "all" properties
            (append (propfind-most-live-properties resource)
                    (propfind-all-dead-properties resource)))
           (propname
            ;; Return the list of available properties
            (list (propstat
                   200
                   ;; car to get tagname, list to construct a valid xml element
                   (map (compose list car)
                        (append
                         (dead-properties resource)
                         (live-properties resource))))))
           (prop
            ;; Return the properties listed
            (propfind-selected-properties
             resource
             (map car (cdr prop))))
           (else
            (scm-error 'bad-request "parse-propfind"
                       "Invalid search query ~s" (list sxml) (list sxml)))))))
